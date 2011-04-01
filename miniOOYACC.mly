/* File miniOOYACC.mly */

%{



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
  VARIABLE VAR SEMICOLON cmd          { Decl($2, $4) }
| expr LPAREN expr RPAREN             { Rpc($1, $3) }
| MALLOC LPAREN VAR RPAREN            { Doa($3) }
| VAR ASSIGN expr                     { Vass($1, $3) }
| expr PERIOD expr ASSIGN expr        { Fass($1, $3, $5) }
| SKIP                                { Skip }
| LCURLYB cmd SEMICOLON cmd RCURLYB   { Sq($2, $4) }
| WHILE bool cmd                      { While($2, $3) }
| IF bool cmd ELSE cmd                { If($2, $3, $5) }
| LCURLYB cmd PARAL cmd RCURLYB       { Para($2, $4) }
| ATOM LPAREN cmd RPAREN              { Atom($3) }

expr :
  FIELD                               { Field($1) }
| VAR                                 { Var($1) }
| NUM                                 { Num($1) }
| NULL                                { Null }
| expr MINUS expr                     { Minus($1, $3) }
| expr PERIOD expr                    { Floc($1, $3) }
| PROC VAR COLON cmd                   { Proc($2, $4) }

bool :
  TRUE                                { True }
| FALSE                               { False }
| expr EQ expr                        { Equal($1, $3) }
| expr LT expr                        { LessThan($1, $3) }

%% 
