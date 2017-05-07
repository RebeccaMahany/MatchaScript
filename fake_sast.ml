open Ast
open Sast

(* hello world *)
let stmt_list : sprogram = []


(* SCallExpr((SId(("print", string)), [SStringLit("hello world")], Void)) *)

let create_fake_sast = 
	stmt_list