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
  | Minus
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
  | Lt
  ;;

type node_attr =
    { mutable eattr : bool option;
      mutable vattr : bool option
    } ;;


type abstract_node =
    ExpressionNode of expression * node_attr * abstract_node list option
  | CommandNode of command * node_attr * abstract_node list option
  | BoolNode of abool * node_attr * abstract_node list option
  | Variable of string * node_attr
  | Field of string * node_attr
  | Number of int * node_attr
  | Null of node_attr
  ;;

let get_eattr e =
  match e with
    ExpressionNode (_, attr, _) -> attr.eattr
  | CommandNode    (_, attr, _) -> attr.eattr
  | BoolNode       (_, attr, _) -> attr.eattr
  | Variable       (_, attr   ) -> attr.eattr
  | Field          (_, attr   ) -> attr.eattr
  | Number         (_, attr   ) -> attr.eattr
  | Null           (attr      ) -> attr.eattr
  ;;

let null_attr = 
  { eattr = Nothing;
    vattr = Nothing
  } ;;

let print_spec = function
    Decl  -> print_string "Declaration "
  | Rpc   -> print_string "RecursivePC "
  | Doa   -> print_string "DynamicAlloc"
  | Vass  -> print_string "VarAssign   "
  | Fass  -> print_string "FieldAssign "
  | Skip  -> print_string "skip        "
  | Sq    -> print_string "Sequence    "
  | While -> print_string "While       "
  | If    -> print_string "If          "
  | Para  -> print_string "Parallel    "
  | Atom  -> print_string "AtomAction  "
  ;;

let rec print_tree = function
    [] -> ()
  | h::t -> 
      match h with 
        CommandNode(spec, _, Some list) ->
          print_spec spec;
          print_tree list
      | ExpressionNode(spec, _, Some list) ->
          print_spec spec;
          print_tree list
      | 
      
    

%}

%token EQ COLON LT MINUS LCURLYB RCURLYB SEMICOLON ASSIGN PERIOD THEN
%token LPAREN RPAREN NULL TRUE FALSE VARIABLE MALLOC PROC SKIP EOL
%token WHILE IF ELSE PARAL ATOM
%token < int > NUM
%token < string > VAR FIELD
%type <unit> prog
%type <abstract_node> cmd 
%type <abstract_node> expr
%type <abstract_node> bool
%left ASSIGN
%left MINUS 
%left PERIOD
%start prog

%%

prog :
  cmd EOL                             { () } 

cmd :
  VARIABLE VAR SEMICOLON cmd          { CommandNode(Decl , null_attr, Some [Variable($2, null_attr); $4])  }
| expr LPAREN expr RPAREN             { CommandNode(Rpc  , null_attr, Some [$1; $3]) }
| MALLOC LPAREN VAR RPAREN            { CommandNode(Doa  , null_attr, Some [Variable($3, null_attr)]) }
| VAR ASSIGN expr                     { CommandNode(Vass , null_attr, Some [Variable($1, null_attr); $3]) }
| expr PERIOD expr ASSIGN expr        { CommandNode(Fass , null_attr, Some [$1; $3; $5] ) }
| SKIP                                { CommandNode(Skip , null_attr, Nothing) }
| LCURLYB cmd SEMICOLON cmd RCURLYB   { CommandNode(Sq   , null_attr, Some [$2; $4]) }
| WHILE bool THEN cmd                 { CommandNode(While, null_attr, Some [$2; $4]) }
| IF bool THEN cmd ELSE cmd           { CommandNode(If   , null_attr, Some [$2; $4; $6]) }
| LCURLYB cmd PARAL cmd RCURLYB       { CommandNode(Para , null_attr, Some [$2; $4]) }
| ATOM LPAREN cmd RPAREN              { CommandNode(Atom , null_attr, Some [$3]) }

expr :
  FIELD                               { Field         ($1   , null_attr) }
| VAR                                 { Variable      ($1   , null_attr) }
| NUM                                 { Number        ($1   , null_attr) }
| NULL                                { Null          null_attr }
| expr MINUS expr                     { ExpressionNode(Minus, null_attr, Some [$1; $3]) }
| expr PERIOD expr                    { ExpressionNode(Floc , null_attr, Some [$1; $3]) }
| PROC VAR COLON cmd                  { ExpressionNode(Proc , null_attr, Some [Variable($2, null_attr); $4]) }

bool :
  TRUE                                { BoolNode(True , null_attr, Nothing) }
| FALSE                               { BoolNode(False, null_attr, Nothing) }
| expr EQ expr                        { BoolNode(Eq   , null_attr, Some [$1; $3]) }
| expr LT expr                        { BoolNode(Lt   , null_attr, Some [$1; $3]) }

%% 
