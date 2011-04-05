(* File miniOO.ml *)
open Parsing;;
try
  let lexbuf = Lexing.from_channel stdin in
  while true do
    try
      MiniOOYACC.prog MiniOOLEX.token lexbuf
    with Parse_error ->
      (print_string "Syntax error ..." ; print_newline ()) ;
    clear_parser ()
  done
with MiniOOLEX.Eof ->
  ()
;;
