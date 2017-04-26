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

type typ = Int | Float | Bool | Char | String | Void

type bind = typ * string

type expr =
    IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | CharLit of char
  | StringLit of string
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of expr * expr (* check this *)
  | Call of string * expr list
  | Noexpr

and stmt =
  | Block of stmt list
  | Expr of expr
  | DeclStmt of typ * string * expr (* How to handle DeclStmt with empty expression? *) 
  | FDeclStmt of fdecl
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

and fdecl = {
  returnType : typ;
  fname : string;
  formals : bind list;
  body : constructs;
}

and constructs = {
  stmts: stmt list;
}

(* type program = include_stmt list * constructs *)
type program = constructs

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

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | FloatLit(f) -> string_of_float f
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | CharLit(c) -> "\'" ^ String.make 1 c ^ "\'"
  | StringLit(s) -> "\"" ^ s ^ "\""
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(e1, e2) -> string_of_expr e1 ^ " = " ^ string_of_expr e2
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let string_of_typ = function
    Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Char -> "char"
  | String -> "string"
  | Void -> "void"

let string_of_bind (t, id) = string_of_typ t ^ " " ^ id

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | DeclStmt(typ, str, expr) -> if expr = Noexpr then string_of_typ typ ^ " " ^ str ^ ";\n"
                                else string_of_typ typ ^ " " ^ str ^ " = " ^ string_of_expr expr ^ ";\n"
  | FDeclStmt(fd) -> string_of_fdecl fd
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s  

and string_of_fdecl fdecl =
	let string_of_constructs constructs = 
	  String.concat "" (List.map string_of_stmt constructs.stmts) 
  (*^ String.concat "" (List.map string_of_fdecl constructs.fdecls)*) in

  "function " ^ string_of_typ fdecl.returnType ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_bind fdecl.formals) ^
  ")\n{\n" ^
   (string_of_constructs fdecl.body) ^
  "}\n"
 
let string_of_program (*(stmts, fdecls)*) stmts =
  String.concat "" (List.map string_of_stmt stmts) ^ "\n" (*^
  String.concat "\n" (List.map string_of_fdecl fdecls)*)