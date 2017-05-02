module S = Sast
module A = Ast
module E = Exceptions

(* Data structures *)

(* Symbol table: *)
type symbol_table = {
	parent: symbol_table option; (* option means a parent scope is optional *)
	mutable st_vdecls: A.vdecl list;
	mutable st_fdecls: A.fdecl list;
	mutable st_cdecls: A.cdecl list;
}

(******************************************************
 * Environment in which we're doing semantic checking
 ******************************************************)
type translation_env = {
	scope : symbol_table;		(* tracks in-scope vars, functions, and classes *)
	return_type : A.datatype; 	(* function's return type *)
	(* in_switch : bool; *)
	(* case - see 4/3/2017 1:06:26 *)
	(* break_label *)
	(* continue_label *)
	(* exception scope? *)
	(* labels on statements *) (* ref keyword makes field mutable *)
	(* forward_gotos *)
}

(***********************************
 * Builtin variables and functions
 ***********************************)

(******************
 * Error messages
 ******************)

(***********
 * Scoping
 ***********)

(*********
 * Utils
 *********)
(* Look for the name within the current symbol table and its parents. *)
let rec find_variable (scope : symbol_table) name (* check_only_this_scope? *)=
	try
		(* a vdecl is (datatype, string, expr) *)
		List.find (fun (_, s, _) -> s = name) scope.variables 
	with Not_found ->
		match scope.parent with
			Some(parent) -> find_variable parent name
		| _ -> raise Not_found
;;

(* find_function *)

(* find_class *)

(*********************
 * Check Expressions
 *********************)
let rec check_expr env expr = match expr with
		A.IntLit(i) 					-> S.SIntLit(i)
	| A.FloatLit f        	-> S.SFloatLit(f)
	| A.BoolLit b       		-> S.SBoolLit(b)
	| A.CharLit c          	-> S.SCharLit(c)
	| A.StringLit s        	-> S.SStringLit(s)
	| A.Id s                -> check_id env id
	| A.This                -> check_this env

	| A.Binop(e1, op, e2)   -> check_binop env e1 op e2
	| A.Unop(op, e)         -> check_unop env op e
	| A.Assign(e1, e2)      -> check_assign env e1 e2

	| A.ObjCreate(s, el)		-> check_obj_create env s el
	| A.ObjAccess(e1, e2)		-> check_obj_access env e1 e2
	| A.CallExpr(e, el)			-> check_call_expr env e el
	| A.Noexpr              -> S.SNoexpr

and check_id env id =
	(* Verify that identifier id is in scope and return its type *)
	let vdecl = try
		find_variable env.scope id (* locate a variable by name *)
	with Not_found ->
		raise E.UndefinedID(id)
 	in
	let (datatype, _, _) = vdecl in (* get variable's type *)
	S.SId(vdecl, datatype) (* !!! vdecl, datatype or string, datatype? *)

and check_this env =

and check_binop env e1 op e2 =

and check_unop env e =

and check_assign env e1 e2 =

and check_obj_create env s el =

and check_obj_access env e1 e2 = 

and check_call_expr env e el =

(********************
 * Check Statements
 ********************)
and check_stmt env stmt = match stmt with
	  A.Block sl						-> S.check_block env sl
	| A.Expr e 							-> S.check_expr_stmt env e
	| A.VarDecl v 					-> S.check_var_decl_stmt env v
  | A.FunDecl f 					-> S.check_fun_decl_stmt env f
  | A.ClassDecl c 				-> S.check_class_decl_stmt env c
  | A.Return e 						-> S.check_return env e
  | A.If(e, s_if, s_el)		-> S.check_if env e s_if s_el
  | A.For(e1, e2, e3, s)	-> S.check_for env e1 e2 e3 s
  | A.While(e, s)					-> S.check_while env e s
  | A.Break								-> S.check_break env e
  | A.Continue						-> S.check_continue env

and check_block env sl = (* 4/3 1:08:00 *)
	(* Create new scope: parent is the existing scope, new scope starts empty *)
	let new_scope = { 
		parent: Some(env.scope); 
		st_vdecls: [];
		st_fdecls: [];
		st_cdecls: [];
	}
	(* no exceptions yet *)

	(* New environment: same, but with new symbol tables *)
	let new_env = { env with scope = new_scope; }

	(* Check all the statements in the block *)
	let sl = List.map (fun s -> stmt new_env s) sl in
	new_scope.st_vdecls <- List.rev new_scope.st_vdecls;

	S.Block(new_scope, sl) (* return block with symbols *)

	(* At end of block, current scope is deleted *)


and check_expr_stmt env e = 

and check_var_decl_stmt env v =
	(* check if v has already been declared - using find_variable? *)

	in
	(* side-effect: add variable to the env symbol table *)
	env.scope.st_vdecls <- v :: env.scope.st_vdecls; (*!!! right way to add it? see 4/3 1:06:00*)

and check_fun_decl_stmt env f =

and check_class_decl_stmt env c =

and check_return env e =

and check_if env e s_if s_el =

and check_for env e1 e2 e3 s =

and check_while env e s =

and check_break env e =

and check_continue env =

(***********************
 * Convert AST to SAST
 ***********************)
let rec convert_ast_to_sast env =


(***********************
 * Program entry point
 ***********************)
let check_ast ast prog = match prog with
	Program(stmts) -> 
		(* Generate sast of SProgram *)
		let _, sast = convert_ast_to_sast env stmts   