/* File miniOOYACC.mly */

%{
exception Fail of string;;

type 'a option =
    Some of 'a
  | None
  ;;

(* The vattr is the attribute of the AST nodes which shows the visible variables the node has. *)

type vattr = string list option;;

(* Use mutual recursive types to define the AST nodes *)

type acmd = 
    Decl of string * acmd * vattr
  | Rpc of aexpr * aexpr * vattr
  | Doa of string * vattr
  | Vass of string * aexpr * vattr
  | Fass of aexpr * aexpr * aexpr * vattr
  | Skip
  | Sq of acmd * acmd * vattr
  | While of abool * acmd * vattr
  | If of abool * acmd * acmd * vattr
  | Para of acmd * acmd * vattr
  | Atom of acmd * vattr
  | Block of acmd
  | Empty

and aexpr =
    Field of string
  | Number of int
  | Minus of aexpr * aexpr * vattr
  | Null
  | Var of string * vattr
  | Floc of aexpr * aexpr * vattr
  | Proc of string * acmd * vattr

and abool =
    True
  | False
  | Eq of aexpr * aexpr * vattr
  | Lt of aexpr * aexpr * vattr
  ;;

(* The expand function expand the v set by a new element s *)

let expand s v =
  match v with 
    Some l -> Some (s :: l)
  | _      -> raise (Fail "expand: the v is nothing")
  ;;

(* setX is to build a new decorated AST with the V attributes by traveling the original AST from top to bottom *)
(* The difference between the original tree and new one is the new tree consists of actual V attributes but the*)
(* original one set the V attributes to None. *)  

let rec setc v node =
  match node with
    Decl(s, c, _) ->
      let new_v = expand s v in 
      Decl(s, setc new_v c, v)
  | Rpc(e1, e2, _) -> Rpc(sete v e1, sete v e2, v)
  | Doa(s, _) -> Doa(s, v)
  | Vass(s, e, _) -> Vass(s, sete v e, v)
  | Fass(e1, e2, e3, _) -> Fass(sete v e1, sete v e2, sete v e3, v)
  | Skip -> Skip
  | Sq(c1, c2, _) -> Sq(setc v c1, setc v c2, v)
  | While(b, c, _) -> While(setb v b, setc v c, v)
  | If(b, c1, c2, _) -> If(setb v b, setc v c1, setc v c2, v)
  | Para(c1, c2, _) -> Para(setc v c1, setc v c2, v)
  | Atom (c, _) -> Atom(setc v c, v)
  | _ -> raise (Fail "setc: invalid pattern in static check stage")
and sete v node =
  match node with
    Field s -> Field s
  | Number i -> Number i
  | Var (s, _) -> Var(s, v)
  | Null -> Null
  | Minus(e1, e2, _) -> Minus(sete v e1, sete v e2, v)
  | Floc(e1, e2, _) -> Floc(sete v e1, sete v e2, v)
  | Proc(s, c, _) ->
      let new_v = expand s v in
      Proc(s, setc new_v c, v)
and setb v node =
  match node with
    True -> True
  | False -> False
  | Eq(e1, e2, _) -> Eq(sete v e1, sete v e2, v)
  | Lt(e1, e2, _) -> Lt(sete v e1, sete v e2, v)
  ;;

(* not_belong takes e v as arguments return true is e does not belong to v and vice versa *)

let rec not_belong e v =
  match v with
  | Some list -> 
      ( match list with
      | [] -> true
      | h :: t -> if e = h then false else not_belong e (Some t) )
  | None -> raise (Fail "not_belong: v is None")
  ;;

(* getX is to get the error attributes which depend on the V attributes of the decorated AST nodes *)

let rec getc node =
  match node with
    Decl(_, c, _) -> getc c
  | Rpc(e1, e2, _) -> gete e1 or gete e2
  | Doa(s, v) -> not_belong s v
  | Vass(s, e, v) -> not_belong s v or gete e
  | Fass(e1, e2, e3, _) -> gete e1 or gete e2 or gete e3
  | Skip -> false
  | Sq(c1, c2, _) -> getc c1 or getc c2
  | While(b, c, _) -> getb b or getc c
  | If(b, c1, c2, _) -> getb b or getc c1 or getc c2
  | Para(c1, c2, _) -> getc c1 or getc c2
  | Atom (c, _) -> getc c
  | _ -> raise (Fail "getc: invalid pattern in static check stage")
and gete node =
  match node with
    Field _ -> false
  | Number _ -> false
  | Null -> false
  | Var(s, v) -> not_belong s v
  | Minus(e1, e2, _) -> gete e1 or gete e2
  | Floc(e1, e2, _) -> gete e1 or gete e2
  | Proc(_, c, _) -> getc c
and getb node =
  match node with
    (True | False) -> false
  | Eq(e1, e2, _) -> gete e1 or gete e2
  | Lt(e1, e2, _) -> gete e1 or gete e2
  ;;

(* static_check is to combine the getX and setX to do the static checking of the AST *)

let static_check root =
  let decorated = setc (Some []) root in
  getc decorated ;;

(* print_result is to print the static semantics checking results out *)

let print_result root =
  let result = static_check root in
  if result then print_string "Error" else print_string "Correct" ;;

(* print_ctrl : acmd -> unit *)
let rec print_ctrl cmd =
  match cmd with
    Decl(s, c, _) -> 
      print_string "var ";
      print_string s;
      print_string "; ";
      print_ctrl c;
  | Rpc(e1, e2, _) ->
      print_expr e1;
      print_string "(";
      print_expr e2;
      print_string ")"
  | Doa(s, _) ->
      print_string "malloc(";
      print_string s;
      print_string ")"
  | Vass(s, e, _) ->
      print_string s;
      print_string " = ";
      print_expr e;
  | Fass(e1, e2, e3, _) ->
      print_expr e1;
      print_string ".";
      print_expr e2;
      print_string " = ";
      print_expr e3
  | Skip ->
      print_string "skip"
  | Sq(c1, c2, _) ->
      print_string "{";
      print_ctrl c1;
      print_string "; ";
      print_ctrl c2;
      print_string "}"
  | While(b, c, _) ->
      print_string "while ";
      print_bool b;
      print_string " then ";
      print_ctrl c;
  | If(b, c1, c2, _) ->
      print_string "if ";
      print_bool b;
      print_string " then ";
      print_ctrl c1;
      print_string " else ";
      print_ctrl c2;
  | Para(c1, c2, _) ->
      print_string "{ ";
      print_ctrl c1;
      print_string " ||| ";
      print_ctrl c2;
      print_string " }"
  | Atom(c, _) ->
      print_string "atom(";
      print_ctrl c;
      print_string ")"
  | Block c ->
      print_string "<block(";
      print_ctrl c;
      print_string ")>"
  | Empty -> ()
and print_expr expr =
  match expr with
    Field s -> 
      print_string s
  | Number i ->
      print_string (string_of_int i)
  | Var(s, _) ->
      print_string s
  | Null -> 
      print_string "null"
  | Minus(e1, e2, _) ->
      print_expr e1;
      print_string " - ";
      print_expr e2
  | Floc(e1, e2, _) ->
      print_expr e1;
      print_string ".";
      print_expr e2
  | Proc(s, c, _) ->
      print_string "proc ";
      print_string s;
      print_string ":";
      print_ctrl c;
and print_bool bool =
  match bool with
    True -> 
      print_string "true"
  | False ->
      print_string "false"
  | Eq(e1, e2, _) ->
      print_expr e1;
      print_string " == ";
      print_expr e2
  | Lt(e1, e2, _) ->
      print_expr e1;
      print_string " < ";
      print_expr e2
  ;;
      
    

(* make_indent : int -> unit *)
let rec make_indent indent =
  match indent with
    0 -> ();
  | i when i < 0 -> raise (Fail "make_indent: the indent cannot be negtive!")
  | _ -> 
      print_string " ";
      make_indent (indent - 1)
  ;;

(* ============== Transitional Semantics Part ================*)

(* Definition of semantics domain *)

(* boolean definition *)
type booleans = 
    BoolTrue
  | BoolFalse
  | BoolError
  ;;

(* object definition *)
type objects = Obj of int;;

(* location definition *)
type locations =
    Loc of objects
  | LocNull
  ;;

(* variable definition *)
type var = V of string ;;

(* Environment definition *)
type env = Env of var * objects;;

(* frame and stack definitions *)
type frame = 
    Declare of env
  | Call of env * stack
and stack =
    frame list
  ;;
(* closure definition *)
type closures = Closure of var * acmd * stack;;

(* field definitions, the Val should have belonged to Field, *)
(* use seperate definition to make program easier. *)
type field = 
    F of string
  | Val
  ;;

(* value definition *)
type values =
    VField of field
  | Int of int
  | Location of locations
  | Clo of closures
  ;;

(* tainted value definition *)
type tvalues =
    Value of values
  | TvError
  ;;

(* heap entry definitions, the heap will be a list of entries *)
type heap = Heap of (field ref * tvalues ref) list ref;;

(* ================ definitions of operations =============== *)


(* heap_initialize: unit -> heap list ref                     *)
(* initialize a empty heap                                    *)
let heap_initialize () = ref ([] : heap list);;

(* newObj: heap list ref -> int                               *)
(* find an available location in heap. Since in this version  *)
(* the garbage collector has not yet been implemented so the  *)
(* dirty locations will not be reclaimed. So an available     *)
(* location will be always the next location, i.e., length of *)
(* the heap.                                                  *)
let newObj heap = 
  let i = List.length !heap in
  Obj i ;;

(* getVal: objects -> heap list -> tvalues                    *)
(* get the [objects] location in the heap and if the Field is *)
(* Val, return the according tainted value.                   *)
let getVal obj heap = 
   match obj with
    Obj n ->
      (match List.nth heap n with
        Heap lref -> 
          ( match !lref with
            [] -> raise (Fail "getVal: the list of field X Tva pairs is empty!")
          | h :: t -> 
              ( match h with
                (fref, tva_ref) ->
                  ( match !fref with
                    Val -> !tva_ref
                  | _ -> raise (Fail "getVal: the request field is NOT val") ))))
  ;;
      
(* initialize a empty stack *)
let stack_initialize () = ref ([] : stack) ;;

(* push a new frame on top of the stack *)
let push s frame = s := frame :: !s ;;

(* pop a most recently frame from stack *)
let pop s =
  match !s with
    [] -> raise (Fail "The stack empty!")
  | h :: t -> s := t
  ;;

(* dump the stack content for debuging *)
(* s_dump : stack -> unit *)
let s_dump indent stack =
  let print_env env =
      match env with
        Env (var, obj) ->
          ( match var, obj with
            V s, Obj i -> 
              print_string s;
              print_string " -> ";
              print_string (string_of_int i) ) in
  
  let rec print_stack indent count stack =
    match stack with
      [] -> ();
    | h :: t -> 
        make_indent indent;
        print_string "stack[";
        print_string (string_of_int count);
        print_string "] ";
        print_frame indent h;
        print_newline ();
        print_stack indent (count + 1) t 
  and print_frame indent frame =
    match frame with
      Declare env ->
        print_string "Decl ";
        print_env env;
    | Call (env, stack) -> 
        print_string "Call ";
        print_env env; 
        print_newline ();
        print_stack (indent + 9) 0 stack in
  
(* stack_dump body starts from here *)
  print_stack indent 0 stack ;;

(* stack_dump: stack -> unit *)
let stack_dump stack = s_dump 0 stack ;;

(* heap_dump : heap list -> unit *)
(* dump the heap for debugging *)
let heap_dump heap =
  let rec print_map mlist =
    let print_field f =
      match f with
        Val -> print_string "val"
      | F s -> 
          print_string s;
          print_string "  " in
    
    let print_tvalue tva = 
      let print_location l =
        match l with
          LocNull -> print_string "null"
        | Loc o ->
            ( match o with
              Obj i -> 
                print_string "heap[";
                print_string (string_of_int i);
                print_string "]" ) in
      let print_closure clo =
        let print_var var =
          match var with
            V s -> print_string s in
        
        match clo with
          Closure (var, ctrl, stack) ->
            print_string "clo< ";
            print_var var;
            print_string ", ";
            print_newline ();
            print_string "              ";
            print_ctrl ctrl;
            print_string ",";
            print_newline ();
            s_dump 14 stack;
            print_string "         >" in
      
      match tva with
        TvError -> print_string "error"
      | Value v -> 
          ( match v with
            VField f -> print_field f
          | Int i -> print_string (string_of_int i)
          | Location l -> print_location l
          | Clo clo -> print_closure clo ) in
    
    match mlist with
      [] -> ()
    | h :: t -> 
        ( match h with
          (rf, rtva) -> 
            print_field !rf;
            print_string "  ->  ";
            print_tvalue !rtva;
            print_newline ();
            print_map t ) in
  
  let rec print_heap count hlist =
    match hlist with
      [] -> ()
    | h :: t -> 
        ( match h with
          Heap fvlist_ref -> 
            print_string "heap[";
            print_string (string_of_int count);
            print_string "]";
            print_newline ();
            print_map !fvlist_ref;
            print_heap (count + 1) t ) in
  
  (* heap_dump body begins from here *)
  print_heap 0 heap ;;

(* lookup: string -> stack -> objects                     *)
(* lookup a variable in the stack and return the non-null     *)
(* locations(objects) on the heap.                            *)
let rec lookup x stack =
  let is_there s env =
    match env with
      Env(v, o) -> 
        ( match v with
          V s -> if s = x then true else false ) in
  let obj env =
    match env with
      Env(_, o) -> o in
  
(* lookup body starts from here *)
  match stack with
    [] -> raise (Fail "lookup failed to find the variable")
  | h :: t ->
      ( match h with
        Declare env -> if is_there x env then obj env else lookup x t
      | Call(env, _) -> if is_there x env then obj env else lookup x t )
  ;;

(* in_loc : tvalues -> heap list -> bool *)
(* Is there an l exist in loc(h) *)
let in_loc tvl heap =
  let lmax = List.length heap in
  match tvl with
    Value v -> 
      ( match v with
        Location l -> 
          ( match l with
            LocNull -> false
          | Loc lc ->
              ( match lc with
                Obj i -> if i < lmax then true else false ) )
      | _ -> raise (Fail "is_exist : the tvalue should be an locations") )
  | _ -> raise (Fail "is_exist : the tvalue should not be TvError in this function")
  ;;

(* is_field : tvalues -> bool *)
let is_field tvl =
  match tvl with
    Value v ->
      ( match v with
        VField _ -> true
      | _ -> false )
  | _ -> false
  ;;

let get_field tvf =
  match tvf with
    Value v -> 
      ( match v with
        VField f -> f
      | _ -> raise (Fail "get_field: tvf must be a field") )
  | _ -> raise (Fail "get_field: tvf must be a value")
  ;;

(* in_dom_h : tvalues -> tvalues -> heap list -> tvalues *)
(* in_dom_h returns true if there is an <l,f> belonging to dom(h) *)
let in_dom_h l f heap =
  let get_int_loc loc =
    match loc with
      Value v ->
        ( match v with
          Location lc ->
            ( match lc with
              Loc o ->
                ( match o with
                  Obj i -> i ) 
            | LocNull -> raise (Fail "get_int_loc: the location cannot be locNull" ) )
        | _ -> raise (Fail "get_int_loc: the tvalues must be a location") )
    | _ -> raise (Fail "get_int_loc: the tvalues must be a values") in
  
 
  
  (* the main function begins from here *)
  let n = get_int_loc l in
  let field = get_field f in
  let lmax = List.length heap in
  if n >= lmax then false 
  else
    match List.nth heap n with
      Heap lref -> 
        ( match !lref with
          [] -> true
        | h :: t -> 
            ( match h with 
              (rf, tva) -> 
                if field = Val && !rf != Val then false else true ) )
                  
    ;;

(* eval : aexpr -> stack -> heap list -> tvalues *)     
(* evaluate a expression with a given state *)

let rec eval expr stack heap =
  match expr with
    Field f -> Value(VField(F f))
  | Number i -> Value(Int i)
  | Var(x, _) -> 
      let loc = lookup x stack in
      getVal loc heap
  | Minus(e1, e2, _) -> 
      let tv1 = eval e1 stack heap in
      let tv2 = eval e2 stack heap in
      ( match tv1, tv2 with
        Value v1, Value v2 -> 
          ( match v1, v2 with
            Int i1, Int i2 -> Value(Int (i1 - i2))
          | _, _ -> TvError )
      | _, _ -> TvError )
  | Null -> Value(Location LocNull)
  | Floc(e1, e2, _) -> 
      let l = eval e1 stack heap in
      let f = eval e2 stack heap in
      ( match (in_loc l heap) && (is_field f) && (in_dom_h l f heap) with
      | false -> TvError
      | true -> TvError  )
  | Proc(s, cmd, _) -> Value( Clo( Closure( V s, cmd, stack) ) )
  ;;

(* evalb : abool -> stack -> heap list -> booleans *)
(* evaluate a bool expression with a given state *)
let evalb expr stack heap =
  match expr with
    True -> BoolTrue
  | False -> BoolFalse
  | Eq(e1, e2, _) ->
      let tv1 = eval e1 stack heap in
      let tv2 = eval e2 stack heap in
      ( match tv1, tv2 with
        TvError, _ -> BoolError
      | _, TvError -> BoolError
      | Value v1, Value v2 ->
          ( match v1, v2 with
            Int _, Int _ -> if v1 = v2 then BoolTrue else BoolError
          | Location _, Location _ -> if v1 = v2 then BoolTrue else BoolError
          | Clo _, Clo _ -> if v1 = v2 then BoolTrue else BoolError
          | _, _ -> BoolError ) )
  | Lt(e1, e2, _) ->
      let tv1 = eval e1 stack heap in
      let tv2 = eval e2 stack heap in
      ( match tv1, tv2 with
        TvError, _ -> BoolError
      | _, TvError -> BoolError
      | Value v1, Value v2 ->
          ( match v1, v2 with
            Int i1, Int i2 -> if i1 < i2 then BoolTrue else BoolFalse
          | _, _ -> BoolError ) )
  ;;
    
(* set_heap : objects -> field -> values -> heap -> unit *)
(* this function implement h[<l, f> -> values] *)
let set_heap obj field va heap =
  let rec set_map f v ml =
    match ml with
      h :: t -> 
        ( match h with
          (rf, rv) -> if !rf = f then begin rv := v; true end else set_map f v t )
    | [] -> false in
  match obj with
    Obj n ->
      if n >= List.length !heap then raise (Fail "set_map: the obj exceeds the boundary") 
      else
        let entry = List.nth !heap n in
        ( match entry with
          Heap mapl -> 
            if set_map field va !mapl then () else mapl := !mapl @ [(ref field, ref va)] )
  ;;
  

(* step : acmd -> stack ref -> heap list ref -> acmd *)        
(* the interior step procedure *)
let rec step ctrl stack heap = 
  match ctrl with
    Empty -> Empty
  | Block cmd -> 
      let next = step cmd stack heap in
      ( match next with
        Empty -> 
          ( match !stack with
            [] -> raise (Fail "runtime error: no decl matched with a block, stack empty")
          | h :: t -> 
              ( match h with
                Declare _ -> 
                  pop stack;
                  Empty
              | Call(_, s) ->
                  stack := s;
                  Empty
              | _ -> raise (Fail "runtime error: no decl matched with a block") ) )
      | c -> Block c )
  | Decl(var, cmd, _) -> 
      let l = newObj heap in
      stack := Declare( Env(V var, l) ) :: !stack;
      heap := !heap @ [Heap( ref [ ( ref Val, ref (Value(Location(LocNull))) ) ] )];
      Block cmd
  | Vass(x, e, _) -> 
      let v = eval e !stack !heap in
      if v = TvError 
      then 
        raise (Fail "runtime error") 
      else 
        let l = lookup x !stack in
        set_heap l Val v heap;
        Empty
  | Fass(e1, e2, e3, _) ->
      let l = eval e1 !stack !heap in
      let f = eval e2 !stack !heap in
      if (not (in_loc l !heap)) or (not (is_field f)) or (not (in_dom_h l f !heap))
      then raise (Fail"runtime error")
      else
        let v = eval e3 !stack !heap in
        let obj = 
          ( match l with
            Value tv ->
              ( match tv with
                Location loc ->
                  ( match loc with
                    Loc o -> o 
                  | _ -> raise (Fail "runtime error") )
            | _ -> raise (Fail " runtime error") )
          | _ -> raise (Fail "runtime error") ) in
        let sf = get_field f in
        set_heap obj sf v heap;
        Empty
  | Doa(x, _) -> 
      let o = newObj heap in
      let vloc = Value(Location(Loc o)) in
      let l = lookup x !stack in
      heap := !heap @ [Heap (ref [])];
      set_heap l Val vloc heap;
      Empty
  | Rpc(e1, e2, _) -> 
      let v = eval e1 !stack !heap in
      ( match v with
        Value va ->
          ( match va with
            Clo clo -> 
              ( match clo with
                Closure (var, c, s) ->
                  let obj = newObj heap in
                  let newStack = Call(Env(var, obj), !stack) :: s in
                  let va = eval e2 !stack !heap in
                  heap := !heap @ [Heap (ref [])];
                  set_heap obj Val va heap;
                  stack := newStack;
                  Block c )
          | _ -> raise (Fail "runtime error") )
      | _ -> raise (Fail "runtime error") )
  | Skip -> Empty
  | Sq(c1, c2, ph) -> 
      let next = step c1 stack heap in
      ( match next with
        Empty -> c2
      | c -> Sq(c, c2, ph) )
  | While(b, c, ph) -> 
      ( match evalb b !stack !heap with
        BoolTrue -> Sq(c, While(b, c, ph), ph)
      | BoolFalse -> Empty
      | BoolError -> raise (Fail "runtime error") )
  | If (b, c1, c2, _) -> 
      ( match evalb b !stack !heap with
        BoolTrue -> c1
      | BoolFalse -> c2
      | BoolError -> raise (Fail "runtime error") )
  | _ -> raise (Fail "interprete: to be implemented!")
  ;;

(* The recursive run function *)
let rec run ctrl stack heap =
  let next_strl = step ctrl stack heap in
  match next_strl with
    Empty _ -> print_string "program ends"; print_newline (); stack_dump !stack; heap_dump !heap
  | c ->
      print_newline ();
      print_string " => ";
      print_ctrl c; 
      print_newline ();
      print_string "******************** stack dump ********************";
      print_newline ();
      stack_dump !stack;
      print_string "******************** heap  dump ********************";
      print_newline ();
      heap_dump !heap; 
      run c stack heap
  ;;

(* the miniOO interpreter *)
let interpreter prog = 
  let stack = stack_initialize () in
  let heap = heap_initialize () in
  run prog stack heap ;;

%}

%token EQ COLON LT MINUS LCURLYB RCURLYB SEMICOLON ASSIGN PERIOD THEN
%token LPAREN RPAREN NULL TRUE FALSE VARIABLE MALLOC PROC SKIP EOL
%token WHILE IF ELSE PARAL ATOM
%token < int > NUM
%token < string > VAR FIELD
%type <unit> prog
%type <acmd> cmd 
%type <aexpr> expr
%type <abool> bool
%left ASSIGN
%left MINUS 
%left PERIOD
%start prog

%%

prog :
  cmd EOL                             { print_result $1; print_newline(); print_ctrl $1; print_newline (); interpreter $1} 

cmd :
  VARIABLE VAR SEMICOLON cmd          { Decl   ($2, $4, None)     }
| expr LPAREN expr RPAREN             { Rpc    ($1, $3, None)     }
| MALLOC LPAREN VAR RPAREN            { Doa    ($3, None)         }
| VAR ASSIGN expr                     { Vass   ($1, $3, None)     }
| expr PERIOD expr ASSIGN expr        { Fass   ($1, $3, $5, None) }
| SKIP                                { Skip                      }
| LCURLYB cmd SEMICOLON cmd RCURLYB   { Sq     ($2, $4, None)     }
| WHILE bool THEN cmd                 { While  ($2, $4, None)     }
| IF bool THEN cmd ELSE cmd           { If     ($2, $4, $6, None) }
| LCURLYB cmd PARAL cmd RCURLYB       { Para   ($2, $4, None)     }
| ATOM LPAREN cmd RPAREN              { Atom   ($3, None)         }

expr :
  FIELD                               { Field  $1                 }
| VAR                                 { Var    ($1, None)         }
| NUM                                 { Number $1                 }
| NULL                                { Null                      }
| expr MINUS expr                     { Minus  ($1, $3, None)     }
| expr PERIOD expr                    { Floc   ($1, $3, None)     }
| PROC VAR COLON cmd                  { Proc   ($2, $4, None)     }

bool :
  TRUE                                { True                      }
| FALSE                               { False                     }
| expr EQ expr                        { Eq     ($1, $3, None)     }
| expr LT expr                        { Lt     ($1, $3, None)     }

%% 
