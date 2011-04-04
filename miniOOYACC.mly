/* File miniOOYACC.mly */

%{

type 'a option =
    Some of 'a
  | Nothing
  ;;

type expression =
    Field
  | Var
  | Num
  | Null
  | Minux
  | Floc
  | Proc
  ;;

type command =
  | Decl
  | Rpc
  | Doa
  | Vass
  | Fass
  | Skip
  | Sq
  | While
  | If
  | Para
  | Atom
  ;;

type abool =
    True
  | False
  | Eq
  | LT
  ;;

type node_attr =
    { Error : bool option;
      Var_decl : bool option
    } ;;


type abstract_node =
    ExpressionNode of expression * node_attr * abstract_node list
  | CommandNode of command * node_attr * abstract_node list
  | BoolNode of abool * node_attr * abstract_node list option
  | Variable of string
  | Field of string
  | Number of int
  | Null
  ;;




%}

%token EQ COLON LT MINUS LCURLYB RCURLYB SEMICOLON ASSIGN PERIOD
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

prog:
  cmd EOL                             { () } 

cmd :
  VARIABLE VAR SEMICOLON cmd          { CommandNode(Decl, Null, [(Variable $2); $4])  }
| expr LPAREN expr RPAREN             { CommandNode(Rpc, Null, [$1; $3] }
| MALLOC LPAREN VAR RPAREN            { CommandNode(Doa, Null, [(Variable $3)] }
| VAR ASSIGN expr                     { CommandNode(Vass, [(Variable $1), $3]) }
| expr PERIOD expr ASSIGN expr        { CommandNode(Fass, [$1; $3; $5] ) }
| SKIP                                { CommandNode(Skip, Null, []) }
| LCURLYB cmd SEMICOLON cmd RCURLYB   { CommandNode(Sq, [$2; $4]) }
| WHILE bool cmd                      { CommandNode(While, [$2; $3]) }
| IF bool cmd ELSE cmd                { CommandNode(If, [$2; $3; $5]) }
| LCURLYB cmd PARAL cmd RCURLYB       { CommandNode(Para, [$2; $4] }
| ATOM LPAREN cmd RPAREN              { CommandNode(Atom, [$3]) }

expr :
  FIELD                               { Field($1) }
| VAR                                 { Variable($1) }
| NUM                                 { Num($1) }
| NULL                                { ExpressionNode(Null, Null, []) }
| expr MINUS expr                     { ExpressionNode(Minus, Null, [$1; $3]) }
| expr PERIOD expr                    { ExpressionNode(Floc, Null, [$1; $3]) }
| PROC VAR COLON cmd                  { ExpressionNode(Proc, Null, [(Var $2); $4]) }

bool :
  TRUE                                { BoolNode(True, Null, Nothing) }
| FALSE                               { BoolNode(False, Null, Nothing) }
| expr EQ expr                        { BoolNode(Equal, Null, Some ([$1; $3])) }
| expr LT expr                        { BoolNode(LT, Null, Some [$1; $3]) }

%% 
