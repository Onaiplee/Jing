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
    True
  | False
  | Error
  ;;

type objects = Obj of int;;

type locations =
    Loc of objects
  | Null
  ;;

type env = Lookup of var * objects;;

type frame = 
    Decl of env
  | Call of env * stack
and stack =
    frame list
  ;;

type closures = Closure of var * acmd * stack;;

type values =
    Field of integers
  | Int of int
  | Loc of locations
  | Clo of closures
  ;;

type tvalues =
    Val of values
  | Error
  ;;

type heap = Heap of objects * field * tvalues;;

type states = State of stack * heap;;

type ctrl =
    Cmd of acmd
  | Block of ctrl
  ;;

type configurations =
    Conf of ctrl * state
  | Error
  ;;

(* definitions of operations and type decomposition functions *)

(* initialize a empty heap *)
let heap_initialize () = 
  ref ([], []);;

(* initialize a empty stack *)
let stack_initialize () = ref ([] : stack) ;;

(* pop a most recently frame from stack *)
let pop s =
  match !s with
    [] -> raise (Fail "The stack empty!")
  | h :: t -> s := t
  ;;

(* push a new frame on top of the stack *)
let push s frame = s := frame :: !s ;;
  
      
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
