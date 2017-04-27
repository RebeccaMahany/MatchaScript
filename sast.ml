open Ast

type sexpr = 
	  SIntLit of int
	| SFloatLit of float
	| SBoolLit of bool
	| SCharLit of char
	| SStringLit of string
(*	| SFunExpr of sfexpr *)
	| SId of string * typ
	| SBinop of sexpr * op * sexpr * typ
	| SUnop of uop * sexpr * typ
	| SAssign of sexpr * sexpr * typ
	| SCallExpr of sexpr * sexpr list * typ 
	| SNoexpr

and sstmt = 
	  SBlock of sstmt list
	| SExpr of sexpr * typ
	| SDeclStmt of typ * string * sexpr * typ 
	| SFDeclStmt of sfdecl * typ
	| SReturn of sexpr * typ
	| SIf of sexpr * sstmt * sstmt
	| SFor of sexpr * sexpr * sexpr * sstmt
	| SWhile of sexpr * sstmt

(*TODO: Add support for sfdecl and sfexpr *)

and sconstructs = {
	sstmts: sstmt list;
}

(*TODO: Add support for classes if necessary *)

