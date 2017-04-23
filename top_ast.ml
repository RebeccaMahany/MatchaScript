open Ast

(* prog is a construct *)
let top_ast prog =
	let stmts = prog.stmts
	and fdecls = prog.fdecls in
	(stmts, fdecls)