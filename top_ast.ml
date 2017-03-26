open Ast

let top_ast prog =
	let globals = prog.vdecls
	and statements = prog.stmts
	and functions = prog.fdecls in
  let new_fdecls = List.append [{
																	Ast.typ = Ast.Void;
																	Ast.fname = "main";
																	Ast.formals = [];
																	Ast.locals = [];
																	Ast.body = statements
																}] functions in
  (globals, new_fdecls)