(*open Ast

type sexpr = 
	  SIntLit of int
	| SFloatLit of float
	| SBoolLit of bool
	| SCharLit of char
	| SStringLit of string
	| SFunExpr of sfexpr
	| SId of string * datatype
	| SBinop of sexpr * op * sexpr * datatype
	| SUnop of uop * sexpr * datatype
	| SAssign of sexpr * sexpr * datatype
	| SCallExpr of sexpr * sexpr list * datatype 
	| SNoexpr

and sstmt = 
	  SBlock of sstmt list
	| SExpr of sexpr * datatype
	| SDeclStmt of datatype * string * sexpr * datatype 
	| SFDeclStmt of sfdecl * datatype 
	| SReturn of sexpr * datatype
	| SIf of sexpr * sstmt * sstmt * datatype
	| SFor of sexpr * sexpr * sexpr * sstmt
	| SWhile of sexpr * sstmt

(*TODO: Add support for sfexpr *)

and sfdecl = {
	sfdReturnType : datatype;
	sfdFname : string;
	sfdFormals : bind list;
	sfdBody : sstmt list; (* changing from ast *)
}

and sconstructs = {
	classes: scdecl list;
	sfdecls: sfdecl list; (* separating out for codegen *)
	main: sfdecl;
}

type sprogram = sconstructs
*)