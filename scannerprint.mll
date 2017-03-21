{ open Printf }

let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let int_lit = ['0'-'9']+ 
let float_lit = int_lit '.'? int_lit* (* simplified for now *)
let string_lit = ['\"'] [^'\"']* ['\"']
let char_lit = ['a'-'z' 'A'-'Z'] (* TODO: Decide what is defined as a char *)

rule token = parse
[' ' '\t' '\r' '\n']     { token lexbuf } (* Whitespace *)
| "/*"                     { comment lexbuf } (* Comments *)
| '('      { print_string "LPAREN " }
| ')'      { print_string "RPAREN " }
| '{'      { print_string "LBRACE " }
| '}'      { print_string "RBRACE " }
| ';'      { print_string "SEMI " }
| ','      { print_string "COMMA " }
| '+'      { print_string "PLUS " }
| '-'      { print_string "MINUS "}
| '*'      { print_string "TIMES " }
| '/'      { print_string "DIVIDE " }
| '%'      { print_string "MOD " }
| "++"     { print_string "INCR " }
| "--"     { print_string "DECR " }
| '='      { print_string "ASSIGN " }
| "=="     { print_string "EQ " }
| "!="     { print_string "NEQ " }
| '<'      { print_string "LT " }
| "<="     { print_string "LEQ " }
| ">"      { print_string "GT " }
| ">="     { print_string "GEQ " }
| "&&"     { print_string "AND " }
| "||"     { print_string "OR " }
| "!"      { print_string "NOT " }
| '['      { print_string "LBRACKET " }
| ']'      { print_string "RBRACKET " }
| "if"     { print_string "IF " }
| "else"   { print_string "ELSE " }
| "for"    { print_string "FOR " }
| "in"     { print_string "IN " }
| "each"   { print_string "EACH " }
| "do"     { print_string "DO " }
| "while"  { print_string "WHILE " }
| "return" { print_string "RETURN " }
| "int"    { print_string "INT " }
| "float"  { print_string "FLOAT " }
| "String" { print_string "STRING " }
| "char"   { print_string "CHAR " }
| "fun"     { print_string "FUN " } (* for function type *)
| "null"   { print_string "NULL " }
| "bool"   { print_string "BOOL " }
| "void"   { print_string "VOID " }
| "true"   { print_string "TRUE " }
| "false"  { print_string "FALSE " }
| "const"  { print_string "CONST " }
| "instanceof" { print_string "INSTANCEOF " }
| "let"    { print_string "LET " }
| "eval"   { print_string "EVAL " } (* JS eval function implemented as an operator *)
| "try"    { print_string "TRY " }
| "catch"  { print_string "CATCH " }
| "finally" { print_string "FINALLY " }
| "continue" { print_string "CONTINUE " }
| "break"   { print_string "BREAK " }
| "class"   { print_string "CLASS " } (* for class declaration *)
| "constructor" { print_string "CONSTRUCTOR " } (* for class declaration *)
| "super" { print_string "SUPER " } (* for constructor declaration *)
| "extends" { print_string "EXTENDS " } (* for inheritance *)
| "function" { print_string "FUNCTION " } (* for function declaration *)
| int_lit as lxm { print_string "INT_LIT(int_of_string lxm) " }
| float_lit as lxm { print_string "FLOAT_LIT(float_of_string lxm) " }
| string_lit as lxm { print_string "STRING_LIT(lxm) " }
| char_lit as lxm { print_string "CHAR_LIT(Char.escaped lxm) " }
| id as lxm { print_string "ID(lxm) " }
| _ as char { print_string "FAILURE " }

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