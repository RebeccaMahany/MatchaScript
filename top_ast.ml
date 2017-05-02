open Ast

(* prog is a construct *)
let top_ast prog = match prog with 
	Program(stmts) -> prog