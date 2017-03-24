module A = Ast

let top_ast (vdecls, stmts, fdecls) =
	(* Convert global variables to assignment statements *)
  let globals = vdecls
  and statements = stmts
  and functions = fdecls in

  