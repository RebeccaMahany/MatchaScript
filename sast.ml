open Ast

type sbind = typ * string

type svdecl = typ * string * sexpr

and sexpr = 
    SIntLit of int
	| SFloatLit of float
	| SBoolLit of bool
	| SCharLit of char
	| SStringLit of string
	| SFunExpr of sfexpr 
	| SId of string * typ
	| SBinop of sexpr * op * sexpr * typ
	| SUnop of uop * sexpr * typ
	| SAssign of sexpr * sexpr * typ
	| SCallExpr of sexpr * sexpr list * typ 
	| SNoexpr

and sstmt = 
    SBlock of sstmt list
  | SExprStmt of sexpr
  | SVarDecl of svdecl
  | SFunDecl of sfdecl
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt

and sfexpr = {
  sfeReturnType : typ;
  sfeFormals : sbind list;
  sfeBody: sstmt list;
}

and sfdecl = {
  sfdReturnType : typ;
  sfdFname : string;
  sfdFormals : bind list;
  sfdBody : sstmt list;
}
(*
and scdecl = {
  scname : string;
  scproperties : svdecl list;
}
*)
and sprogram = sstmt list

(* Pretty-printing functions *)
let string_of_sbind (t, id) = string_of_typ t ^ " " ^ id

let rec string_of_sexpr = function
    SIntLit(l) -> string_of_int l
  | SFloatLit(f) -> string_of_float f
  | SBoolLit(b) -> if b then "true" else "false"
  | SCharLit(c) -> "\'" ^ String.make 1 c ^ "\'"
  | SStringLit(s) -> "\"" ^ s ^ "\""
  | SId(s,t) -> s
  | SBinop(e1, o, e2, t) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e, t) -> string_of_uop o ^ string_of_sexpr e
  | SAssign(e1, e2, t) -> string_of_sexpr e1 ^ " = " ^ string_of_sexpr e2
  | SCallExpr(call_expr, args, t) ->
      string_of_sexpr call_expr ^ "(" ^ String.concat ", " (List.map string_of_sexpr args) ^ ")"
  | SNoexpr -> ""
