open Ast
open Sast

module StringMap = Map.Make(String)


(* for testing later *)
let funTwoConstructs = {
	stmts= [Expr(IntLit(2))]
} 
let funTwo = {
  fdReturnType = Void;
  fdFname = "goodbye";
  fdFormals= [];
  fdBody = funTwoConstructs
}

let funOneConstructs = {
	stmts= [FDeclStmt(funTwo)]
} 
let funOne = { 
  fdReturnType = Void;
  fdFname = "hello";
  fdFormals= [];
  fdBody = funOneConstructs
} 
let constructs = {
	stmts= [FDeclStmt(funOne)]
} 
let program = constructs 
(***********************)

(* When calling parse_stmt for the first time at the highest level, call it with an empty StringMap as one of its args. All subsequent functions such as parse_block will pass in this StringMap and add itself to it as necessary (assuming the stmt is valid) except for parse_function. In the case of parse_function, we'll pass in the existing StringMap (which by now may have a few other stmts) as well as an empty StringMap to start a new scope. Right when we enter the parse_function, we push the existing StringMap onto the Stack and then forget about it. We add the fdecl to the new empty map (key is fdecl name, value is fdecl object). We call parse_stmt on this fdecl's body, passing in the new map. Every time right before parse_function ends, pop off the new map (newDict). This is so that the next statement that is checked will be in the right scope. *)

(* TODO: Create the sast part of this while parsing the function. In parse_function, right before the Stack.pop call at the end, convert everything that's on the stack so far as being within the scope of func (that is, within the func's body), and then add that new fdecl to the 'global' fdecl list of the sast *)


(* passing in a function, stack, and dictionary *)
let parse_function func stack oldDict newDict =
	Stack.push oldDict stack;
	 (* Search through all dictionaries on stack to see if func name exists already *)
	let m = StringMap.add func.fdFname func newDict in    (* add function to map *)
	List.iter parse_stmt s m func.fdBody.stmts;
	Stack.pop stack
	
	
and parse_stmt s m = function
	  FDeclStmt f		-> parse_function f s m StringMap.empty
(*	| Block sl		-> parse_block sl *)  (* not implemented yet *)


(* for unit testing parsing a function *)
let s = Stack.create () 
parse_function funOne s StringMap.empty; 
print_int s.length
