open Ast
open Sast

module StringMap = Map.Make(String)

let s = Stack.create () in

let rec parse_function f = function 
	
in 
let parse_stmt = function
	  FDeclStmt f		-> parse_function f
