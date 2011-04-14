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

(* Transitional Semantics Part *)

(* Definition of semantics domain *)

type booleans = 
    BoolTrue
  | BoolFalse
  | BoolError
  ;;

type objects = Obj of int;;

type locations =
    Loc of objects
  | LocNull
  ;;
type var = V of string ;;

type env = Env of var * objects;;

type frame = 
    Declare of env
  | Call of env * stack
and stack =
    frame list
  ;;

type closures = Closure of var * acmd * stack;;

type field = 
    F of string
  | Val
  ;;

type values =
    VField of field
  | Int of int
  | Location of locations
  | Clo of closures
  ;;

type tvalues =
    Value of values
  | TvError
  ;;

type heap = Heap of (field ref * tvalues ref) list ref;;

type states = State of stack ref * heap list ref ;;

type ctrl =
    Cmd of acmd
  | Block of ctrl
  ;;

type configurations =
    Conf of ctrl ref * states ref
  | Final of states ref
  | ConfError
  ;;

(* definitions of operations and type decomposition functions *)


(* heap_initialize: unit -> heap list ref                     *)
(* initialize a empty heap.                                   *)
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

(* getVal: objects -> heap list -> tvalues                *)
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
      | _ -> lookup x t )
  ;;

(* push a new frame on top of the stack *)
let push s frame = s := frame :: !s ;;

(* pop a most recently frame from stack *)
let pop s =
  match !s with
    [] -> raise (Fail "The stack empty!")
  | h :: t -> s := t
  ;;

(* initialize a state *)
let state_initialize () =
  let s = stack_initialize () in
  let h = heap_initialize () in
  ref ( State(s, h) ) ;;

(* initialize a configurations of the program *)
let conf_initialize prog =
  let state = state_initialize () in
  ref ( Conf(prog, state) ) ;;

(* decomposite the heap from current configuration *)
let getHeap conf =
  match !conf with
    Conf(_, s) -> 
      ( match !s with
        State(_, h) -> h ) 
  | Final s -> raise (Fail "getHeap: the computation has been completed!")
  | ConfError -> raise (Fail "getHeap: the state is Error!")
  ;;

(* decomposite the stack from current configuration *)
let getStack conf =
  match !conf with
    Conf(_, s) -> 
      ( match !s with
        State(s, _) -> s ) 
  | Final s -> raise (Fail "getStack: the computation has been completed!")
  | ConfError -> raise (Fail "getStack: the state is Error!")
  ;;
(* decomposite the Ctrl from current configuration *)
let getCtrl conf =
  match !conf with
    Conf(c, _) -> c
  | Final _ -> raise (Fail "getCtrl: the computation has been completed!")
  | ConfError -> raise (Fail "getCtrl: the state is Error!")
  ;;

(* decomposite the current cmd from the configuration *)
let current conf =
  let rec dec c =
    match c with
      Cmd cmd -> cmd
    | Block ctrl -> dec ctrl in
  match !conf with
    Conf(ctrl, _) -> dec !ctrl
  | Final _ -> raise (Fail "The computation has been completed!")
  | ConfError -> raise (Fail "current: the computation got an error!")
  ;;

(* is_exist : tvalues -> heap list -> bool *)
(* Is there an l exist in loc(h) *)
let is_exist tvl heap =
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
    
(* dom_h : tvalues -> tvalues -> heap list -> tvalues *)
(* dom_h returns true if there is an <l,f> belonging to dom(h) *)
let dom_h l f heap =
  let get_int_loc loc =
    match loc with
      Value v ->
        ( match v with
          Location lc ->
            ( match lc with
              Loc o ->
                ( match o with
                  Obj i -> i ) )
        | _ -> raise (Fail "get_int_loc: the tvalues must be a location") )
    | _ -> raise (Fail "get_int_loc: the tvalues must be a values") in
  
  let get_field tvf =
    match tvf with
      Value v -> 
        ( match v with
          VField f -> f
        | _ -> raise (Fail "get_field: tvf must be a field") )
    | _ -> raise (Fail "get_field: tvf must be a value") in

  let rec get_value f list =
    match list with
      [] -> Value(Location(LocNull))
    | h :: t -> 
       ( match h with 
         (fr, tr) -> if f = !fr then !tr else get_value f t ) in
  
  (* the main function begins from here *)
  let n = get_int_loc l in
  let field = get_field f in
  match List.nth heap n with
    Heap lref -> get_value field !lref ;;

(* in_dom_h : tvalues -> bool *)
let in_dom_h v =
  match v with
    Value v ->
      ( match v with
        Location lc ->
          ( match lc with
            LocNull -> false
          | _ -> true )
      | _ -> raise (Fail "in_dom_h: v must be a Location") )
  | _ -> raise (Fail "in_dom_h: v must be a value")
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
      ( match (is_exist l heap) && (is_field f) with
        true ->
          let vl = dom_h l f heap in
          if in_dom_h vl then vl else TvError
      | _ -> TvError )
  | Proc(s, cmd, _) -> Value( Clo( Closure( V s, cmd, stack) ) )
  ;;
        
(* the interior interprete procedure *)
let rec interprete conf = 
  let c = current conf in
  let s = getStack conf in
  let h = getHeap conf in
  let ctrl = getCtrl conf in
  match c with
    Decl(var, cmd, _) -> 
      let l = newObj h in
      s := Declare( Env(V var, l) ) :: !s;
      h := !h @ [Heap( ref [ ( ref Val, ref (Value(Location(LocNull))) ) ] )];
      ctrl := Block( (Cmd cmd) );
  | Vass(x, e, _) -> 
      ( match eval e !s !h with
        TvError -> ();
      | Value v -> (); )
  | _ -> raise (Fail "interprete: to be implemented!")
  ;;

(* the miniOO interpreter *)
let interpreter prog =
  let init_conf = conf_initialize prog in
  interprete init_conf ;;
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
  cmd EOL                             { print_result $1; print_newline() } 

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
