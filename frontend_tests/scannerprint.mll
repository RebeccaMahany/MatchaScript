(* Ocamllex scanner for MicroC *)

{ open Printf }

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
| '('      { print_string "LPAREN " }
| ')'      { print_string "RPAREN " }
| '{'      { print_string "LBRACE " }
| '}'      { print_string "RBRACE " }
| ';'      { print_string "SEMI " }
| ','      { print_string "COMMA " }

(* Operators *)
| '+'      	{ print_string "PLUS " }
| '-'      	{ print_string "MINUS " }
| '*'      	{ print_string "TIMES " }
| '/'      	{ print_string "DIVIDE " }
| '='      	{ print_string "ASSIGN " }
| "=="     	{ print_string "EQ " }
| "!="     	{ print_string "NEQ " }
| '<'		    { print_string "LT " }
| "<="     	{ print_string "LEQ " }
| ">"	      { print_string "GT " }
| ">="     	{ print_string "GEQ " }
| "&&"		  { print_string "AND " }
| "||"		  { print_string "OR " }
| "!"		    { print_string "NOT " }
| '.'       { print_string "DOT " }
| '['       { print_string "LBRACKET " }
| ']'       { print_string "RBRACKET " }

(* Branch control *)
| "if"     { print_string "IF " }
| "else"   { print_string "ELSE " }
| "for"    { print_string "FOR " }
| "while"  { print_string "WHILE " }
| "return" { print_string "RETURN " }

(* Data types *)
| "int"    { print_string "INT " }
| "float"  { print_string "FLOAT " }
| "char"   { print_string "CHAR " }
| "string" { print_string "string "}
| "bool"   { print_string "BOOL " }
| "void"   { print_string "VOID " }
| "true"   { print_string "TRUE " }
| "false"  { print_string "FALSE " }

(* Literals *)
| int as lxm 	{ print_string "INTLIT " }
| float as lxm { print_string "FLOATLIT " }
| id as lxm 	{ print_string "ID " }
| string 		  { print_string "STRINGLIT " }
| char as lxm   { print_string "CHARLIT " }
(*| eof 			{ EOF }*)
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

{
  let main () =
    let lexbuf = Lexing.from_channel stdin in
    try
        while true do
            ignore (token lexbuf)
        done
    with _ -> print_string "invalid_token\n"
  let _ = Printexc.print main ()

}