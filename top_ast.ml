
open Ast

let top_ast prog =
	let globals = prog.vdecls
	(*and statements = List.rev prog.stmts*)
	and body = { vdecls= []; stmts= []; fdecls= [];  }
	and functions = prog.fdecls in
  let new_fdecls = List.append [{
																	Ast.typ = Ast.Void;
																	Ast.fname = "main";
																	Ast.formals = [];
																	Ast.body = body;
																}] functions in
  (globals, new_fdecls)

