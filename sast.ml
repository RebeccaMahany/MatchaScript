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


