/* File miniOOYACC.mly */

%{

type 'a option =
    Some of 'a
  | Nothing
  ;;

type node_spec =
    Minus
  | Floc
  | Proc
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
  | True
  | False
  | Eq
  | Lt
  ;;

(* type command =
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
  ;; *)

type node_attr =
    { mutable eattr : bool option;
      mutable vattr : string list
    } ;;


type abstract_node =
    ExpressionNode of node_spec * node_attr * abstract_node list option
  | CommandNode of node_spec * node_attr * abstract_node list option
  | BoolNode of node_spec * node_attr * abstract_node list option
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
    vattr = []
  } ;;

let print_spec = function
    Decl  -> print_string "Declaration "
  | Rpc   -> print_string "RecursivePC "
  | Doa   -> print_string "DynamicAlloc "
  | Vass  -> print_string "VarAssign "
  | Fass  -> print_string "FieldAssign "
  | Skip  -> print_string "Skip "
  | Sq    -> print_string "Sequence "
  | While -> print_string "While "
  | If    -> print_string "If "
  | Para  -> print_string "Parallel "
  | Atom  -> print_string "AtomAction "
  | Minus -> print_string "Minus "
  | Floc  -> print_string "Floc "
  | Proc  -> print_string "Proc "
  | True  -> print_string "True "
  | False -> print_string "False "
  | Eq    -> print_string "Eq "
  | Lt    -> print_string "Lt "
  ;;

let rec my_map f l =
  match l with
    [] -> ()
  | h :: t -> 
      begin
        (f h);
        my_map f t
      end
        ;;

let rec print_string_list = function
    [] -> ()
  | h :: t -> print_string h; print_string_list t;;


let rec print_tree = function 
    CommandNode(spec, attr, l) ->
      ( match l with
        Some list ->
          begin
            print_spec spec;
            print_string_list attr.vattr;
            my_map print_tree list
          end
      | Nothing ->
          print_spec spec )
  | ExpressionNode(spec, attr, l) ->
      ( match l with
        Some list ->
          begin
            print_spec spec;
            print_string_list attr.vattr;
            my_map print_tree list
          end
      | Nothing -> 
          print_spec spec )
  | BoolNode(spec, attr, l) ->
      ( match l with
        Some list -> 
          begin
            print_spec spec;
            print_string_list attr.vattr;
            my_map print_tree list
          end
      | Nothing ->
          print_spec spec )
  | Variable(s, attr) ->
      begin
        (* print_string s; *)
        print_string " ";
        print_string_list attr.vattr;
      end
  | Field(s, _) -> 
      print_string s
  | Number(i, _) ->
      begin
        (* print_string (string_of_int i); *)
        print_string " "
      end
  | Null _ ->
      print_string "Null "
  ;;

exception Fail of string;;

let rec return n list =
  match list with
    [] -> raise (Fail "return: the list is empty")
  | h :: t -> if n == 0 then h else return (n - 1) t
  ;;

let rec write_back = function
    CommandNode(spec, _, l) ->
      ( match spec, l with
        Decl, Some list -> 
          begin
            print_string "var ";
            write_back (return 0 list);
            print_string "; ";
            write_back (return 1 list)
          end
      | Rpc, Some list ->
          begin
            write_back (return 0 list);
            print_string "( ";
            write_back (return 1 list);
            print_string ") "
          end
      | Doa, Some list ->
          begin
            print_string "malloc(";
            write_back (return 0 list);
            print_string ") "
          end
      | Vass, Some list ->
          begin
            write_back (return 0 list);
            print_string "= ";
            write_back (return 1 list)
          end
      | Fass, Some list ->
          begin
            write_back (return 0 list);
            print_string ".";
            write_back (return 1 list);
            print_string "= ";
            write_back (return 2 list);
          end
      | Skip, _ ->
          print_string "null "
      | Sq, Some list ->
          begin
            print_string "{ ";
            write_back (return 0 list);
            print_string "; ";
            write_back (return 1 list);
            print_string "} "
          end
      | While, Some list ->
          begin
            print_string "while ";
            write_back (return 0 list);
            print_string "then ";
            write_back (return 1 list)
          end
      | If, Some list ->
          begin
            print_string "if ";
            write_back (return 0 list);
            print_string "then ";
            write_back (return 1 list);
            print_string "else ";
            write_back (return 2 list)
          end
      | Para, Some list ->
          begin
            print_string "{ ";
            write_back (return 0 list);
            print_string "||| ";
            write_back (return 1 list);
            print_string "} "
          end
      | Atom, Some list ->
          begin
            print_string "atom(";
            write_back (return 0 list);
            print_string ") "
          end )
  | ExpressionNode(spec, _, l) ->
      ( match spec, l with
        Minus, Some list ->
          begin
            write_back (return 0 list);
            print_string "- ";
            write_back (return 1 list)
          end
      | Proc, Some list ->
          begin
            print_string "proc ";
            write_back (return 0 list);
            print_string ": ";
            write_back (return 1 list)
          end
      | Floc, Some list ->
          begin
            write_back (return 0 list);
            print_string ".";
            write_back (return 1 list);
          end )
  | BoolNode(spec, _, l) ->
      ( match spec, l with
        Eq, Some list ->
          begin
            write_back (return 0 list);
            print_string "== ";
            write_back (return 1 list)
          end
      | Lt, Some list ->
          begin
            write_back (return 0 list);
            print_string "< ";
            write_back (return 1 list)
          end
      | True, Nothing ->
          print_string "true "
      | False, Nothing ->
          print_string "false " )
  | Field(s, _) ->
      begin
        print_string s;
        print_string " "
      end
  | Variable(s, _) ->
      begin
        print_string s;
        print_string " "
      end
  | Number(i, _) ->
      begin
        print_string (string_of_int i);
        print_string " "
      end
  | Null _ ->
      print_string "null "
  ;;

let rec not_belong e s =
  match s with
    [] -> true
  | h :: t -> if e = h then false else not_belong e t
  ;;

let (@@) e s = not_belong e s;;

let getVar = function
    Variable(s, _) -> s
  | _ -> raise (Fail "getVar: the node contains no variable")
  ;;

let rec setv v = function
    CommandNode(spec, attr, l) ->
      ( match spec, l with
        Decl, Some list ->
          begin
            attr.vattr <- v;
            let node = (return 0 list) in
            let newattr = v @ [(getVar node)] in
            begin
              (* print_string_list newattr; *)
              setv newattr (return 1 list);
            end
          end
      | Rpc, Some list ->
          begin
            attr.vattr <- v;
            setv v (return 0 list);
            setv v (return 1 list);
          end
      | Doa, Some list ->
          attr.vattr <- v
      | Vass, Some list ->
          begin
            attr.vattr <- v;
            setv v (return 1 list)
          end
      | Fass, Some list ->
          begin
            attr.vattr <- v;
            setv v (return 0 list);
            setv v (return 1 list);
            setv v (return 2 list)
          end
      | Skip, _ ->
          attr.vattr <- v
      | Sq, Some list ->
          begin
            attr.vattr <- v;
            setv v (return 0 list);
            setv v (return 1 list)
          end
      | While, Some list ->
          begin
            attr.vattr <- v;
            setv v (return 0 list);
            setv v (return 1 list)
          end
      | If, Some list ->
          begin
            (* print_string_list attr.vattr; *)
            attr.vattr <- v; 
            print_string_list attr.vattr;
            setv v (return 0 list);
            setv v (return 1 list);
            setv v (return 2 list);
          end
      | Para, Some list ->
          begin
            attr.vattr <- v;
            setv v (return 0 list);
            setv v (return 1 list)
          end
      | Atom, Some list ->
          begin
            attr.vattr <- v;
            setv v (return 0 list)
          end )
  | ExpressionNode(spec, attr, l) ->
      ( match spec, l with
        Minus, Some list ->
          begin
            attr.vattr <- v;
            setv v (return 0 list);
            setv v (return 1 list)
          end
      | Proc, Some list ->
          begin
            attr.vattr <- v;
            let node = (return 0 list) in
            let newattr = v @ [getVar node] in
            begin
              (* print_string_list newattr; *)
              setv newattr (return 1 list)
            end
          end
      | Floc, Some list ->
          begin 
            attr.vattr <- v;
            setv v (return 0 list);
            setv v (return 1 list)
          end )
  | BoolNode(spec, attr, l) ->
      ( match spec, l with
        True, _ ->
          attr.vattr <- v
      | False, _ ->
          attr.vattr <- v
      | (Eq | Lt), Some list ->
          begin
            attr.vattr <- v;
            setv v (return 0 list);
            setv v (return 1 list)
          end )
  | Field(_, attr) ->
      attr.vattr <- v
  | Variable(_, attr) ->
      attr.vattr <- v
  | Number(_, attr) ->
      attr.vattr <- v
  | Null attr ->
      attr.vattr <- v    
  ;;

let rec gete = function
    CommandNode(spec, attr, l) ->
      ( match spec, l with
        Decl, Some list ->
          gete (return 1 list)
      | Rpc, Some list -> 
          (gete (return 0 list)) or (gete (return 1 list))
      | Doa, Some list ->
          (getVar (return 0 list)) @@ attr.vattr
      | Vass, Some list ->
          let cattr = ((getVar (return 0 list)) @@ attr.vattr) in
          begin
            (* if cattr then print_string "true" else print_string "false"; *)
            cattr or (gete (return 1 list))
          end
      | Fass, Some list ->
          (gete (return 0 list)) or (gete (return 1 list)) or (gete (return 2 list))
      | Skip, _ -> false
      | Sq, Some list ->
          (gete (return 0 list)) or (gete (return 1 list))
      | While, Some list ->
          (gete (return 0 list)) or (gete (return 1 list))
      | If, Some list ->
          begin
            (* print_string_list attr.vattr; *)
            (gete (return 0 list)) or (gete (return 1 list)) or (gete (return 2 list))
          end
      | Para, Some list ->
          (gete (return 0 list)) or (gete (return 1 list))
      | Atom, Some list ->
          gete (return 0 list) )
  | ExpressionNode(spec, attr, l) ->
      ( match spec, l with
        Minus, Some list ->
          (gete (return 0 list)) or (gete (return 1 list))
      | Proc, Some list ->
          begin
            (* print_string_list attr.vattr; *)
            gete (return 1 list)
          end
      | Floc, Some list ->
          (gete (return 0 list)) or (gete (return 1 list)) )
  | BoolNode(spec, attr, l) ->
      ( match spec, l with
        (True | False), _ -> false
      | (Eq | Lt), Some list ->
          begin
            print_string_list attr.vattr;
            (gete (return 0 list)) or (gete (return 1 list))
          end )
  | Variable(s, attr) ->
      let cattr = s @@ attr.vattr in
      begin
        (* print_string_list attr.vattr; *)
        (* print_string "Var: "; *)
        (* print_string s; *)
        (* if cattr then print_string "true" else print_string "false"; *)
        cattr;
      end
  | Field(_, _) -> false
  | Number(_, _) -> false
  | Null _ -> false
  ;;

let static_check node =
  begin
    setv [] node;
    let error = gete node in
    if error then print_string "Error" else print_string "correct";
    print_newline()
end ;;
  
          
          
          
        

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
  cmd EOL                             { (setv [] $1); (print_tree $1); (if (gete $1) then print_string "error" else print_string "correct");  } 

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
