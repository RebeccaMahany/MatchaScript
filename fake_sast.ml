open Ast
open Sast

(* hello world *)
let hello_world : sprogram = [SExprStmt(SCallExpr(SId("print", Fun), [SStringLit("hello world")],
	Void))]

(* empty program *)
let empty_prog : sprogram = []

let create_fake_sast = 
	hello_world
	
let one_plus_one : sprogram = [SVarDecl(Int,"variable_b",SIntLit(1)); SVarDecl(Int,"variable_a",SIntLit(1)); SExprStmt(SBinop(SId("variable_a", Int), Add, SId("variable_b", Int), Int))]
