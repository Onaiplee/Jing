(*   File miniOOLEX.mll                                              *)

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


{
open MiniOOYACC;;
exception Eof;;
} 
rule token = parse
  [' ' '\t']      { token lexbuf } (* skip blanks and tabs *)
| ['\n' ]          { EOL          }
| ['a'-'z' ] as f  { FIELD (Char.escaped f) }
| ['A'-'Z' ] as x  { VAR (Char.escaped x) }
| '1'              { NUM 1        } 
| "=="             { EQ           }
| ':'              { COLON        }
| '<'              { LT           }
| '-'              { MINUS        }
| '{'              { LCURLYB      }
| '}'              { RCURLYB      }
| ';'              { SEMICOLON    }
| '='              { ASSIGN       }
| '.'              { PERIOD       }
| '('              { LPAREN       }
| ')'              { RPAREN       }
| "then"           { THEN         }
| "null"           { NULL         }
| "true"           { TRUE         }
| "false"          { FALSE        }
| "var"            { VARIABLE     }
| "malloc"         { MALLOC       }
| "proc"           { PROC         }
| "skip"           { SKIP         }
| "while"          { WHILE        }
| "if"             { IF           }
| "else"           { ELSE         }
| "|||"            { PARAL        }
| "atom"           { ATOM         }
| eof              { raise Eof    }
