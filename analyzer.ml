module S = Sast
module A = Ast
module E = Exceptions

(* Data structures *)

(* Symbol table: *)
type symbol_table = {
	parent: symbol_table option; (* option means a parent scope is optional *)
		name		: string;
	mutable variables	: A.vdecl list;
		return_type	: A.typ;
	mutable formals		: A.bind list; 
	(*mutable st_fdecls: A.fdecl list;
	mutable st_cdecls: A.cdecl list;*)
}

(******************************************************
 * Environment in which we're doing semantic checking
 ******************************************************)
type translation_env = {
	scope : symbol_table;		(* tracks in-scope vars, functions, and classes *)
	(*name		: string;	(* current function's name *)
	return_type 	: A.typ;	(* current function's return type *)
	formals		: A.bind list;	(* current function's parameters *)
*)	in_for		: bool;		(* whether in for loop context *)
	in_while	: bool;		(* whether in while loop context *)

	(* case - see 4/3/2017 1:06:26 *)
	(* labels on statements *) (* ref keyword makes field mutable *)
}

let update_env_context tenv in_for in_while = {
	scope 		= tenv.scope;
	in_for		= in_for;
	in_while	= in_while;
}

(***********************************
 * Builtin variables and functions
 ***********************************)
 (* printf *)

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
let rec find_vdecl (scope : symbol_table) name (* option to check only current scope? *)=
	try
		(* a vdecl is (typ, string, expr) *)
		List.find (fun (_, s, _) -> s = name) scope.variables 
	with Not_found ->
		match scope.parent with
			Some(parent) -> find_vdecl parent name
		| _ -> raise Not_found

(* find_vdecl *)

(* find_cdecl *)

(*********************
 * Check Expressions
 *********************)
 (*)
let rec check_expr env expr = match expr with
	  A.IntLit(i)						-> S.SIntLit(i)
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
		find_vdecl env.scope id (* locate a variable by name *)
	with Not_found ->
		raise E.UndefinedID(id)
 	in
	let (typ, _, _) = vdecl in (* get variable's type *)
	S.SId(vdecl, typ) (* !!! vdecl, typ or string, typ? *)

and check_this env =
  1

and check_binop env e1 op e2 =
  1

and check_unop env e =
  1

and check_assign env e1 e2 =
  1

and check_obj_create env s el =
  1

and check_obj_access env e1 e2 = 
  1

and check_call_expr env e el =
  1

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
(*
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
*)

and check_expr_stmt env e = 
  1

and check_var_decl_stmt env v =
  1
(*	(* check if v has already been declared - using find_vdecl? *)

	in
	(* side-effect: add variable to the env symbol table *)
	env.scope.st_vdecls <- v :: env.scope.st_vdecls; (*!!! right way to add it? see 4/3 1:06:00*)
*)

*)

(**********************
* Helper functions
**********************)
let get_id_type tenv i =
  let get_vname (_,name,_) = name in
    let vdecl = try List.find (fun x->(get_vname x)=i) tenv.scope.variables 
    with | Not_found -> raise (E.UndeclaredIdentifier(i)) in
      let get_vdecl_typ (typ,_,_) = typ in
        get_vdecl_typ vdecl 
        

let convert_fexpr tenv f =
    (* TODO*)
 {
    S.sfdReturnType = f.A.feReturnType;
    S.sfdFname = (* look at the parent's name *) "anon";
    S.sfdParent = (* look at the parent's name *) "parent";
    S.sfdFormals = f.A.feFormals;
    S.sfdLocals = (* parse fbody for locals *) [];
    S.sfdBody = f.A.feBody;
  };
  S.SIntLit(0)  (* Ignore the result of this *)

let check_binop tenv e1 op e2 = 
  (* TODO *)
  S.SIntLit(0)

let check_unop tenv uop e =
  (* TODO *)
  S.SIntLit(0)

let check_assign tenv e1 e2 = 
  (* TODO *)
  S.SIntLit(0)

(********************
 * Check Expressions
 ********************)
let check_expr tenv = function
	  A.IntLit i		-> S.SIntLit(i), tenv
	| A.BoolLit b		-> S.SBoolLit(b), tenv
	| A.FloatLit f		-> S.SFloatLit(f), tenv
	| A.CharLit c 		-> S.SCharLit(c), tenv
	| A.StringLit s		-> S.SStringLit(s), tenv
	| A.FunExpr f		-> convert_fexpr tenv f, tenv
	(*Here we include fexpr, where we'll convert to fdecl *)
	| A.Id i		-> S.SId(i, get_id_type tenv i), tenv
	| A.Binop(e1,op,e2)	-> check_binop tenv e1 op e2, tenv
	| A.Unop(uop, e)	-> check_unop tenv uop e, tenv
	| A.Assign(e1, e2)	-> check_assign tenv e1 e2, tenv
	| A.Noexpr		-> S.SNoexpr, tenv	

and get_sexpr_type = function 
	  S.SIntLit(_)		-> A.Int
	| S.SBoolLit(_)		-> A.Bool
	| S.SFloatLit(_)	-> A.Float
	| S.SCharLit(_)		-> A.Char
	| S.SStringLit(_)	-> A.String
	| S.SId(_, t)		-> t
	| S.SBinop(_,_,_,t)	-> t
	| S.SUnop(_,_,t)	-> t
	| S.SAssign(_,_,t)	-> t
	| S.SCallExpr(_,_,t)	-> t

(***********************
 * Check Statements
 ***********************)
let rec check_block tenv sl = match sl with
	  [] -> S.SBlock([S.SExprStmt(S.SNoexpr)]), tenv (* empty block *)
	| _ -> let sl, _ = check_stmt_list tenv sl in 
	  S.SBlock(sl), tenv 

(* Check exprstmt by just checking expr *)
and check_expr_stmt tenv e =
  let sexpr, tenv = check_expr tenv e in
    S.SExprStmt(sexpr), tenv

(* TODO: add vdecl to symbol table *)
and check_vdecl tenv v =
  let get_v_expr (_,_,e) = e in (* helper to get expr from vdecl tuple *)
    let vexpr = get_v_expr v in
      let vsexpr, _ = check_expr tenv vexpr in
        let vstyp = get_sexpr_type vsexpr in
          let get_v_typ (t,_,_) = t in  (* helper to get typ of vdecl *)
            let vtyp = get_v_typ v in
              let get_v_name (_,n,_) = n in
                let vname = get_v_name v in
                  if vtyp = vstyp then S.SVarDecl(v), tenv
                    else raise(E.VariableDeclarationTypeMismatch(vname)) 

and check_fdecl tenv f =
  (* TODO *)
  S.SExprStmt(S.SIntLit(0)), tenv

and check_return tenv e =
  let sexpr, _ = check_expr tenv e in
    let t = get_sexpr_type sexpr in
      if t = tenv.scope.return_type then S.SReturn(sexpr), tenv
      else raise (E.ReturnTypeMismatch(A.string_of_typ t, A.string_of_typ tenv.scope.return_type))

and check_if tenv e s1 s2 =
  let pred, _ = check_expr tenv e in
    let t = get_sexpr_type pred in
      let ifstmt, _ = check_stmt tenv s1 in
        let elsestmt, _ = check_stmt tenv s2 in
          if t = A.Bool then S.SIf(pred, ifstmt, elsestmt), tenv
          else raise(E.InvalidIfStatementCondition)

and check_for tenv e1 e2 e3 s =
  (* TODO *)
  S.SExprStmt(S.SIntLit(0)), tenv

and check_while tenv e s =
  (* TODO *)
  S.SExprStmt(S.SIntLit(0)), tenv

and check_stmt tenv = function
	  A.Block sl		-> check_block tenv sl
	| A.ExprStmt e		-> check_expr_stmt tenv e
	| A.VarDecl v		-> check_vdecl tenv v  
	| A.FunDecl f		-> check_fdecl tenv f
	| A.Return e		-> check_return tenv e
	| A.If(e, s1, s2)	-> check_if tenv e s1 s2
	| A.For(e1, e2, e3, s)	-> check_for tenv e1 e2 e3 s
	| A.While(e,s)		-> check_while tenv e s

(* To be used as entrypoint for parsing ast, which is a stmt list *)
and check_stmt_list tenv stmt_list =
  let ref = ref(tenv) in		(* create a pointer to env *)
    let rec iter = function	(* dereference and modify env on each pass *) 
      h::t ->
        let sstmt, tenv = check_stmt !ref h in
          ref := tenv; sstmt::(iter t)
    | [] -> [] 
    in
      let sstmt_list = (iter stmt_list), !ref in
      sstmt_list		(* parse each statement in the stmt list *)

	


(***********************
 * Convert AST to SAST
 ***********************)
let rec convert_ast_to_sast env prog =
  1
  (*print_string "in convert_ast_to_sast\n"*)

  (* return SAST *)

(***********************
 * Program entry point
 ***********************)
let root_symbol_table : symbol_table = {
  parent	 = None;
  name		 = "anon"; 
  variables	 = [];
  return_type	 = A.Void;
  formals	 = [];
 (* st_fdecls = []; (* add builtins *)
  st_cdecls = []; *)
}

let root_env : translation_env = {
  scope = root_symbol_table;
  (*return_type = A.Int;   (* Int 0? *) *)
  in_for = false;
  in_while = false;
}

let check_ast ast = match ast with
	A.Program(stmts) -> 
    (* Generate sast of SProgram *)
    convert_ast_to_sast root_env ast

(* Testing *)
let test_ok sast = match sast with
  _ -> "okay\n"
