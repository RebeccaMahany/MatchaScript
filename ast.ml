(* Abstract Syntax Tree *)

(* Style Guide:
    AST:
      - typecategory
      - SpecificType
    Parser:
      - TERMINAL
      - non_terminal
*)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

type typ = Int | Bool | Str | Void

type bind = typ * string

type vdecl = typ * string * expr

type expr =
    IntLit of int
  | BoolLit of bool
  | StrLit of string
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type stmt =
    Vdecl of vdecl
  | Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type fdecl = {
  typ : typ;
  fname : string;
  formals : bind list;
  locals : vdecl list;
  body : stmt list;
}

type constructs = {
  vdecls: vdecl list;
  stmts: stmt list;
  fdecls: fdecl list;
}

type program = Program of vdecl list * stmt list * fdecl list