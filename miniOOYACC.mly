(* File miniOOYACC.mly *)

%{  (* header *)

%}  (* declarations *)

%token EQ COLON LT MINUS LCURLYB RCURLYB SEMICOLON ASSIGN PERIOD
%token LPAREN RPAREN NULL TRUE FALSE VAR MALLOC PROC SKIP
%token WHILE IF ELSE PARAL ATOM
%token < int > NUM
%token < stirng > ID
%start prog

%% (* rules *)

expr :
  ID
| NUM
| expr MINUS expr
| NULL
| expr PERIOD expr
| PROC ID COLON cmd

cmd :
  VAR ID SEMICOLON cmd
| expr LPAREN expr RPAREN
| MALLOC LPAREN ID RPAREN
| ID ASSIGN expr
| expr PERIOD expr ASSIGN expr
| SKIP
| LCURLYB cmd SEMICOLON cmd RCURLYB
| WHILE bool cmd
| IF bool cmd ELSE cmd
| LCURLYB cmd PARAL cmd RCURLYB
| ATOM LPAREN cmd RPAREN

bool :
  TRUE
| FALSE
| expr EQ expr
| expr LT expr

(*                    The syntax of the miniOO                       *)

(*   p, ..., x, ...   E   Var                                        *)
(*           f, ...   E   Field                                      *)
(*                e   E   Exp                                        *)
(*                e  ::=  f                                          *)
(*                     |  1 | e - e | ...                            *)
(*                     |  null | x | e.e                             *)
(*                     |  proc y:C                                   *)
(*                b   E   Bool                                       *)
(*                b  ::=  true | false | e == e | e < e | ...        *)
(*                                                                   *)
(*                C   E   Cmd                                        *)
(*                C  ::=  var x;C                                    *)
(*                     |  e(e)                                       *)
(*                     |  malloc(x)                                  *)
(*                     |  x = e                                      *)
(*                     |  e.e = e                                    *)
(*                     |  skip | {C;C} | while b C | if b C else C   *)
(*                     |  {C|||C} | atom(C)                          *)
