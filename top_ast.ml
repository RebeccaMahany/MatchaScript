open Ast

(* prog is a construct *)
let top_ast prog =
	let stmts = prog.stmts in
	(*and fdecls = prog.fdecls in
	(stmts, fdecls)*)
	stmts