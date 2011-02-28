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
open miniOOYACC;;
} 
rule token = parse
  [' ' '\t' '\n']  { token lexbuf } (* skip blanks and tabs *)
| ['a'-'z'] as idt { ID idt       } (* idt can be var or f depends on syntax *)
| '1'              { Int 1        } 
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
| "null"           { NULL         }
| "true"           { TRUE         }
| "false"          { FALSE        }
| "var"            { VAR          }
| "malloc"         { MALLOC       }
| "proc"           { PROC         }
| "skip"           { SKIP         }
| "while"          { WHILE        }
| "if"             { IF           }
| "else"           { ELSE         }
| "|||"            { PARAL        }
| "atom"           { ATOM         }
| eof              { raise Eof    }
