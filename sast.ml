open Ast

<<<<<<< HEAD
type sexpr = 
	  SIntLit of int
=======
type sbind = typ * string

type svdecl = typ * string * expr

type sexpr = 
    SIntLit of int
>>>>>>> ae7d5340e051e25abd64bef95f8885fcdbc92b6d
	| SFloatLit of float
	| SBoolLit of bool
	| SCharLit of char
	| SStringLit of string
<<<<<<< HEAD
(*	| SFunExpr of sfexpr *)
=======
	| SFunExpr of sfexpr
>>>>>>> ae7d5340e051e25abd64bef95f8885fcdbc92b6d
	| SId of string * typ
	| SBinop of sexpr * op * sexpr * typ
	| SUnop of uop * sexpr * typ
	| SAssign of sexpr * sexpr * typ
<<<<<<< HEAD
=======
	| SObjCreate of string * sexpr list * typ
	| SObjAccess of sexpr * sexpr * typ
	(* Array access *)
>>>>>>> ae7d5340e051e25abd64bef95f8885fcdbc92b6d
	| SCallExpr of sexpr * sexpr list * typ 
	| SNoexpr

and sstmt = 
<<<<<<< HEAD
	  SBlock of sstmt list
	| SExpr of sexpr * typ
	| SDeclStmt of typ * string * sexpr * typ 
	| SFDeclStmt of sfdecl * typ 
	| SReturn of sexpr * typ
	| SIf of sexpr * sstmt * sstmt
	| SFor of sexpr * sexpr * sexpr * sstmt
	| SWhile of sexpr * sstmt

(*TODO: Add support for sfexpr *)

and sfdecl = {
	sfdReturnType : typ;
	sfdFname : string;
	sfdFormals : bind list;
	sfdBody : sstmt list; (* changing from ast *)
}

and sconstructs = {
	sstmts: sstmt list;
	sfdecls: sfdecl list; (* separating out for codegen *)
}

(*TODO: Add support for classes if necessary *)

type sprogram = sconstructs

=======
    SBlock of sstmt list
  | SExprStmt of sexpr
  | SVarDecl of svdecl
  | SFunDecl of sfdecl
  | SClassDecl of scdecl
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
  sfdBody : stmt list;
}

and scdecl = {
  scname : string;
  scproperties : svdecl list;
}
>>>>>>> ae7d5340e051e25abd64bef95f8885fcdbc92b6d
