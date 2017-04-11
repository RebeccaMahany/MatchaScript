(* Ocamllex scanner for MicroC *)

{ open Parser }

let alpha = ['a'-'z' 'A'-'Z']
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let escape_char = ''' (escape) '''
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let digit = ['0'-'9']
let id = alpha (alpha | digit | '_')*
let string = '"' ( (ascii | escape)* as s) '"'
let char = ''' ( ascii | digit ) '''
let float = (digit+) ['.'] digit+
let int = digit+
let whitespace = [' ' '\t' '\r' '\n']

rule token = parse
  whitespace { token lexbuf }
| "/*"     { comment lexbuf }
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
| '.'      { DOT }
| '['      { LBRACKET }
| ']'      { RBRACKET }

(* Branch control *)
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }

(* Data types *)
| "int"    { INT }
| "float"  { FLOAT }
| "bool"   { BOOL }
| "String" { STRING }
| "void"   { VOID }
| "true"   { TRUE }
| "false"  { FALSE }

(* Literals *)
| int as lxm 	{ INTLIT(int_of_string lxm) }
| float as lxm  { FLOATLIT(float_of_string lxm) }
| id as lxm 	{ ID(lxm) }
| string 		{ STRLIT(s) }
| eof 			{ EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
