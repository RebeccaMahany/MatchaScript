(* Ocamllex scanner for MatchaScript *)

{ open Parser }

let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let int_lit = ['1'-'9']['0'-'9']* 
let float_lit = int_lit '.'? int_lit* (* simplified for now *)
let string_lit = ['\"'] [^'\"']* ['\"']
let char_lit = ['a'-'z' 'A'-'Z'] (* TODO: Decide what is defined as a char *)

rule token = parse
  [' ' '\t' '\r' '\n']     { token lexbuf } (* Whitespace *)
| "/*"                     { comment lexbuf } (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '%'      { MOD }
| "++"     { INCREMENT }
| "--"     { DECREMENT }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "in"     { IN }
| "each"   { EACH }
| "do"     { DO }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "float"  { FLOAT }
| "String" { STRING }
| "char"   { CHAR }
| "fun"     { FUN } (* for function type *)
| "null"   { NULL }
| "bool"   { BOOL }
| "void"   { VOID }
| "true"   { TRUE }
| "false"  { FALSE }
| "const"  { CONST }
| "instanceof" { INSTANCEOF }
| "let"    { LET }
| "eval"   { EVAL } (* JS eval function implemented as an operator *)
| "try"    { TRY }
| "catch"  { CATCH }
| "finally" { FINALLY }
| "continue" { CONTINUE }
| "break"   { BREAK }
| "class"   { CLASS } (* for class declaration *)
| "constructor" { CONSTRUCTOR } (* for class declaration *)
| "function" { FUNCTION } (* for function declaration *)
| int_lit as lxm { INT_LIT(int_of_string lxm) }
| float_lit as lxm { FLOAT_LIT(float_of_string lxm) }
| string_lit as lxm { STRING_LIT(lxm) }
| char_lit as lxm { CHAR_LIT(Char.escaped lxm) }
| id as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/"                     { token lexbuf }
| _                        { comment lexbuf }
