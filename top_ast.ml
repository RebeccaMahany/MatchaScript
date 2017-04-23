open Ast

(* prog is a construct *)
let top_ast prog =
	let stmts = List.rev prog.stmts
	and fdecls = prog.fdecls in
	print_string (String.concat " " (List.map Ast.string_of_stmt stmts));
  (stmts, fdecls)