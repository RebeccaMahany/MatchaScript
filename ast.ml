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

type typ = Int | Float | Bool | Char | Void | ObjectType of string | Fun | String

type bind = typ * string

and vdecl = typ * string * expr

and expr =
    IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | CharLit of char
  | StringLit of string
  | Array of expr list
  | FunExpr of fexpr 
  | Id of string
  | This
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of expr * expr
  | ObjCreate of string * expr list
  | ObjAccess of expr * expr
  | CallExpr of expr * expr list (* callee * arguments *)
  | MemberExpr of expr * [ `Id of string | `ExprStmt of expr ]
  | Noexpr

and stmt =
  | Block of stmt list
  | ExprStmt of expr
  | VarDecl of vdecl
  | FunDecl of fdecl
  | ClassDecl of cdecl
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

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

(* Classes *)
and cbody = {
  properties : vdecl list;
  constructors : constr list;
  methods : fdecl list;
}

and cdecl = { 
  cname : string;
  (* extends *)
  cbody: cbody;
}

and constr = { 
  formals : bind list;
  body : stmt list;
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
  | ObjectType(o) -> "class " ^ o 

let string_of_bind (t, id) = string_of_typ t ^ " " ^ id

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | FloatLit(f) -> string_of_float f
  | BoolLit(b) -> if b then "true" else "false"
  | CharLit(c) -> "\'" ^ String.make 1 c ^ "\'"
  | StringLit(s) -> "\"" ^ s ^ "\""
  | FunExpr(f) -> string_of_fexpr f
  | Id(s) -> s
  | This -> "this"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(e1, e2) -> string_of_expr e1 ^ " = " ^ string_of_expr e2
  | ObjCreate(classname, args) -> "new " ^ classname ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"
  | ObjAccess(e1, e2) -> string_of_expr e1 ^ "." ^ string_of_expr e2
  | CallExpr(call_expr, args) ->
      string_of_expr call_expr ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"
  | Noexpr -> ""

and string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | ExprStmt(expr) -> string_of_expr expr ^ ";\n"
  | VarDecl(v) -> string_of_vdecl v
  | FunDecl(fd) -> string_of_fdecl fd
  | ClassDecl(cdecl) -> string_of_cdecl cdecl
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s  

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
 
and string_of_method mth = 
  string_of_typ mth.fdReturnType ^ " " ^
  mth.fdFname ^ "(" ^ String.concat ", " (List.map string_of_bind mth.fdFormals) ^
  ")\n{\n" ^ String.concat "" (List.map string_of_stmt mth.fdBody) ^ "}"

and string_of_constr constr =
  "constructor(" ^ String.concat ", " (List.map string_of_bind constr.formals) ^ ") {" 
  ^ String.concat "" (List.map string_of_stmt constr.body) ^ "}\n" 

and string_of_cbody cbody = 
  String.concat "" (List.map string_of_vdecl cbody.properties) ^ 
  String.concat "" (List.map string_of_constr cbody.constructors) ^ String.concat "" 
  (List.map string_of_method cbody.methods)

and string_of_cdecl cdecl =
  "class " ^ cdecl.cname ^ "{\n" ^ string_of_cbody cdecl.cbody ^ "}\n"

let string_of_program prog = match prog with 
  Program(stmts) -> String.concat "" (List.map string_of_stmt stmts) ^ "\n"