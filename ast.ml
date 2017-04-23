(* Abstract Syntax Tree *)

(* Style Guide:
    AST:
      - typecategory
      - SpecificType
    Parser:
      - TERMINAL
      - non_terminal
*)

type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

type typ = Int | Float | Bool | Char | Void | Fun | String

type bind = typ * string

and vdecl = typ * string * expr

and expr =
    IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | CharLit of char
  | StringLit of string
  | FunExpr of fexpr
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of expr * expr
  | CallExpr of expr * expr list 
  | Ternary of expr * expr * expr (* Ternary operator ?: *)
  | Noexpr

and stmt =
  | Block of stmt list
  | ExprStmt of expr
  | VarDecl of vdecl
  | FunDecl of fdecl
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | DoWhile of stmt * expr
  | Break

and fexpr = {
  feReturnType : typ;
  feFormals : bind list;
  feBody: stmt list;
}

and fdecl = {
  fdReturnType : typ;
  fdFname : string;
  fdFormals : bind list;
  fdBody : stmt list;
}

(* type program = include_stmt list * constructs *)
type program = Program of stmt list

(* Pretty-printing functions *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let string_of_typ = function
    Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Char -> "char"
  | String -> "string"
  | Void -> "void"
  | Fun -> "fun"

let string_of_bind (t, id) = string_of_typ t ^ " " ^ id

let rec string_of_expr = function
    IntLit(i) -> string_of_int i
  | FloatLit(f) -> string_of_float f
  | BoolLit(b) -> if b then "true" else "false"
  | CharLit(c) -> "\'" ^ String.make 1 c ^ "\'"
  | StringLit(s) -> "\"" ^ s ^ "\""
  | FunExpr(f) -> string_of_fexpr f
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(e1, e2) -> string_of_expr e1 ^ " = " ^ string_of_expr e2
  | Ternary(p, e1, e2) -> string_of_expr p ^ " ? " ^ string_of_expr e1 ^ " : " ^ string_of_expr e2
  | CallExpr(call_expr, args) ->
      string_of_expr call_expr ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"
  | Noexpr -> ""

and string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | ExprStmt(expr) -> string_of_expr expr ^ ";\n"
  | VarDecl(v) -> string_of_vdecl v
  | FunDecl(fd) -> string_of_fdecl fd
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s  
  | DoWhile(s, e) -> "do: " ^ string_of_stmt s ^ " while (" ^ string_of_expr e ^ ")"
  | Break -> "break"

and string_of_vdecl (typ, str, expr) = 
  if expr = Noexpr then string_of_typ typ ^ " " ^ str ^ ";\n"
  else string_of_typ typ ^ " " ^ str ^ " = " ^ string_of_expr expr ^ ";\n"

and string_of_fexpr fexpr =
  "function " ^ string_of_typ fexpr.feReturnType ^ " " 
  ^ "(" ^ String.concat ", " (List.map string_of_bind fexpr.feFormals) ^
  ")\n{\n" ^ String.concat "" (List.map string_of_stmt fexpr.feBody) ^ "}"

and string_of_fdecl fdecl =
  "function " ^ string_of_typ fdecl.fdReturnType ^ " " ^
  fdecl.fdFname ^ "(" ^ String.concat ", " (List.map string_of_bind fdecl.fdFormals) ^
  ")\n{\n" ^ String.concat "" (List.map string_of_stmt fdecl.fdBody) ^ "}"

let string_of_program prog = match prog with 
  Program(stmts) -> String.concat "" (List.map string_of_stmt stmts) ^ "\n"
