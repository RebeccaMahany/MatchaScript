(* Ocamllex scanner for MicroC *)

{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }

(* Operators *)
| '+'      	{ PLUS }
| '-'      	{ MINUS }
| '*'      	{ TIMES }
| '/'      	{ DIVIDE }
| '='      	{ ASSIGN }
| "=="     	{ EQ }
| "!="     	{ NEQ }
| '<'		{ LT }
| "<="  	{ LEQ }
| ">"	    { GT }
| ">="     	{ GEQ }
| "&&"		{ AND }
| "||"		{ OR }
| "!"		{ NOT }

(* Branch control *)
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }

(* Data types *)
| "int"    { INT }
| "bool"   { BOOL }
| "void"   { VOID }
| "true"   { TRUE }
| "false"  { FALSE }

(* Classes *)

| ['0'-'9']+ as lxm { INTLIT(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
