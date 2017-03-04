(* Ocamllex scanner for MatchaScript *)

{ open Parser }

(* To-Do: Implement rules for parsing a float *)
rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
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
| "double" { DOUBLE }
| "String" { STRING }
| "char"   { CHAR }
| "null"   { NULL }
| "bool"   { BOOL }
| "void"   { VOID }
| "true"   { TRUE }
| "false"  { FALSE }
| "const"  { CONST }
| "let"    { LET }
| "eval"   { EVAL } (* JS eval function implemented as an operator *)
| "try"    { TRY }
| "catch"  { CATCH }
| "finally" { FINALLY }
| "continue" { CONTINUE }
| "break"   { BREAK }
| "class"   { CLASS } (* for class declaration *)
| "constructor" { CONSTRUCTOR } (* for class declaration *)
| "fun"     { FUN } (* for function declaration *)
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
