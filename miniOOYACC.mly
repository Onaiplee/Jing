/* File miniOOYACC.mly */

%{

%}

%token EQ COLON LT MINUS LCURLYB RCURLYB SEMICOLON ASSIGN PERIOD
%token LPAREN RPAREN NULL TRUE FALSE VAR MALLOC PROC SKIP
%token WHILE IF ELSE PARAL ATOM
%token < int > NUM
%token < stirng > ID
%type <unit> cmd expr bool
%right MINUS 
%left PERIOD
%start cmd

%% 

cmd :
  VAR ID SEMICOLON cmd                { () }
| expr LPAREN expr RPAREN             { () }
| MALLOC LPAREN ID RPAREN             { () }
| ID ASSIGN expr                      { () }
| expr PERIOD expr ASSIGN expr        { () }
| SKIP                                { () }
| LCURLYB cmd SEMICOLON cmd RCURLYB   { () }
| WHILE bool cmd                      { () }
| IF bool cmd ELSE cmd                { () }
| LCURLYB cmd PARAL cmd RCURLYB       { () }
| ATOM LPAREN cmd RPAREN              { () }

expr :
  ID                                  { () }
| NUM                                 { () }
| NULL                                { () }
| expr MINUS expr                     { () }
| expr PERIOD expr                    { () }
| PROC ID COLON cmd                   { () }

bool :
  TRUE                                { () }
| FALSE                               { () }
| expr EQ expr                        { () }
| expr LT expr                        { () }

%% 
