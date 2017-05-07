open Ast
open Sast

(* hello world *)
let hello_world : sprogram = [SExprStmt(SCallExpr(SId("print", Fun), [SStringLit("hello world")],
	Void))]

(* empty program *)
let empty_prog : sprogram = []

let create_fake_sast = 
	empty_prog