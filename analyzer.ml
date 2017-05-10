module S = Sast
module A = Ast
module E = Exceptions
module Str = Str
module StringMap = Map.Make(String)

type symbol_table = {
	parent: symbol_table option; (* option means a parent scope is optional *)
		name		: string;
	mutable variables	: A.vdecl list;
		return_type	: A.typ;
	mutable formals		: A.bind list;
	mutable	fun_names	: string list;
}

(******************************************************
 * Environment in which we're doing semantic checking
 ******************************************************)
type translation_env = {
	scope : symbol_table;		(* tracks in-scope vars, functions, and classes *)
	in_for		: bool;		(* whether in for loop context *)
	in_while	: bool;		(* whether in while loop context *)
}

let update_env_context tenv in_for in_while = {
	scope 		= tenv.scope;
	in_for		= in_for;
	in_while	= in_while;
}


(**********************
* Helper functions
**********************)
let rec check_call_helper (scope : symbol_table) name =  
    if (scope.name <> name)	(* search for function name in scope *)
    then match scope.parent with 
      Some(parent) -> check_call_helper parent name
    | _ -> raise Not_found 
    else name

let check_call_wrapper tenv i =
  let res = try check_call_helper tenv.scope i 
   with Not_found -> "A.Void" in 
     res

let rec find_variable (scope : symbol_table) name =  (* takes in a scope of type symbol_table *)
  try List.find (fun (_,n,_) -> n = name) scope.variables
  with Not_found ->
    match scope.parent with 
      Some(parent) -> find_variable parent name (* keep searching each parent's scope if not found until there's no more parent *)
    | _ -> raise Not_found

let rec find_formal (scope : symbol_table) name =  (* takes in a scope of type symbol_table *)
  try List.find (fun (_,n) -> n = name) scope.formals
  with Not_found ->
    match scope.parent with 
      Some(parent) -> find_formal parent name (* keep searching each parent's scope if not found until there's no more parent *)
    | _ -> raise Not_found

let find_fun_vdecl (scope : symbol_table) name =
  try List.find (fun (_,n,_) -> n = name) scope.variables
  with Not_found -> raise Not_found

let check_call_fun_vdecls tenv i =  (* [identifying hello in] fun hello = function fun... *) 
   let vdecl = try find_fun_vdecl tenv.scope i
    with | Not_found -> (A.Void, "Not Found", A.IntLit(0)) in
      let get_vdecl_typ (typ,_,_) = typ in
         get_vdecl_typ vdecl

let find_fdecl (scope : symbol_table) name =
  try List.find (fun x -> x = name) scope.fun_names
  with Not_found -> raise Not_found

let find_parent_fdecl (scope : symbol_table) name =
  match scope.parent with 
  | Some(parent) -> 
      try List.find (fun x -> x = name) parent.fun_names
      with Not_found -> raise Not_found
  | _ -> raise Not_found

let check_call_fun tenv i =
  let res = try find_fdecl tenv.scope i 
  with Not_found -> try find_parent_fdecl tenv.scope i with
   Not_found -> "A.Void" in
    res

let search_vdecls tenv i = 
   let vdecl = try find_variable tenv.scope i
    with | Not_found -> (A.Void, "Not Found", A.IntLit(0)) in
      let get_vdecl_typ (typ,_,_) = typ in
         get_vdecl_typ vdecl

let search_fdecls tenv i =
  let fname = try find_fdecl tenv.scope i
  with | Not_found -> "Void" in
  if fname = "Void" then A.Void else A.Fun

let search_parent_fdecls tenv i =
  let fname = try find_parent_fdecl tenv.scope i
  with | Not_found -> "Void" in
  if fname = "Void" then A.Void else A.Fun

let search_formals tenv i =
   let formal = try find_formal tenv.scope i
     with | Not_found -> (A.Void, "Void") in
       let get_formal_typ (typ,_) = typ in
         get_formal_typ formal

let get_id_type tenv i =
  let typ = search_vdecls tenv i in
  if typ = A.Void 
  then begin
   let ft = search_fdecls tenv i in
     if ft = A.Void 
     then (let t = search_formals tenv i in
           if t = A.Void 
           then (let t = search_parent_fdecls tenv i in
                 if t = A.Void then raise(E.UndeclaredIdentifier(i))
                 else t)
           else t) 
     else ft
  end 
  else typ
 
(* helper for check_fdecl, checks for fdecl name dup *) 
let rec find_fdecl_name (scope : symbol_table) name =  (* takes in a scope of type symbol_table *)
  if (scope.name <> name)
  then match scope.parent with 
      Some(parent) -> find_fdecl_name parent name
    | _ -> raise Not_found 
  else raise(E.DuplicateFunction(name))

let rec check_block tenv sl = match sl with
	  [] -> S.SBlock([S.SExprStmt(S.SNoexpr)]), tenv (* empty block *)
	| _ -> let sl, _ = check_stmt_list tenv sl in 
	  S.SBlock(sl), tenv 

and check_fexpr tenv f =
  let get_bind_string (_,s) = s in
  let formals_map = List.fold_left (fun m formal ->  (* check formal dups within current function *)
    if StringMap.mem (get_bind_string formal) m 
    then raise(E.DuplicateFormal(get_bind_string formal))
    else StringMap.add (get_bind_string formal) formal m) StringMap.empty f.A.feFormals in    
  let scope' = (* create a new scope *)
    	{ 
	parent = Some(tenv.scope); (* parent may or may not exist *)
	variables = []; 
	name = "anon"; (* anonymous function *)
	return_type = f.A.feReturnType;
	formals = f.A.feFormals;
	fun_names = [];
	} in
  let tenv' = 
	{ tenv with scope = scope'; } in
  let get_ssl (sstmt_list, _) = sstmt_list in
  let sslp = check_stmt_list tenv' f.A.feBody in scope'.variables <- List.rev scope'.variables;
  let sfexpr = {
	S.sfeReturnType = f.A.feReturnType;
	S.sfeFormals = f.A.feFormals;
	S.sfeBody = get_ssl sslp; 
} in S.SFunExpr(sfexpr)

(* numbers and string concatenation are supported *)
and check_math_binop se1 op se2 = function 
	| (A.Int, A.Float)
	| (A.Float, A.Int)
	| (A.Float, A.Float) -> S.SBinop(se1, op, se2, A.Float)
	| (A.Int, A.Int) -> S.SBinop(se1, op, se2, A.Int)
	| _ -> raise(E.InvalidBinopEvalType)

and check_equal_binop se1 op se2 t1 t2 = (* no floats or functions *)
  if (t1 = A.Float || t2 = A.Float || t1 = A.Fun || t2 = A.Fun) 
  then raise(E.InvalidBinopEvalType)
  else if t1 = t2 then S.SBinop(se1, op, se2, A.Bool)
       else raise(E.InvalidBinopEvalType)

and check_compare_binop se1 op se2 = function (* only numbers supported *)
	| (A.Int, A.Float) 
	| (A.Float, A.Int)
	| (A.Float, A.Float)	
	| (A.Int, A.Int)	-> S.SBinop(se1, op, se2, A.Bool)
	| _ -> raise(E.InvalidBinopEvalType)

and check_bool_binop se1 op se2 = function (* only boolean supported *)
	| (A.Bool, A.Bool)	-> S.SBinop(se1, op, se2, A.Bool)
	| _ -> raise(E.InvalidBinopEvalType) 

and check_binop tenv e1 op e2 = 
  let se1, _ = check_expr tenv e1 in
  let se2, _ = check_expr tenv e2 in
  let t1 = get_sexpr_type se1 in
  let t2 = get_sexpr_type se2 in
  match op with
	  A.Add | A.Sub | A.Mult | A.Div | A.Mod	-> check_math_binop se1 op se2 (t1, t2)
	| A.Equal | A.Neq				-> check_equal_binop se1 op se2 t1 t2
	| A.Less | A.Leq | A.Greater | A.Geq    	-> check_compare_binop se1 op se2 (t1, t2)
	| A.And | A.Or					-> check_bool_binop se1 op se2 (t1, t2)
	| _ -> raise(E.InvalidBinaryOperator)

and check_unop tenv uop e =
  let se, _ = check_expr tenv e in
  let t = get_sexpr_type se in
  let check_num_unop uop se = function (* numbers only *)
	| A.Int -> S.SUnop(uop, se, A.Int)
	| A.Float -> S.SUnop(uop, se, A.Float)
	| _ -> raise(E.InvalidUnopEvalType) in
  let check_bool_unop uop se = function (* bool only *)
	| A.Bool -> S.SUnop(uop, se, A.Bool)
	| _ -> raise(E.InvalidUnopEvalType) in
  match uop with
	  A.Neg		-> check_num_unop uop se t
	| A.Not		-> check_bool_unop uop se t
	| _		-> raise(E.InvalidUnaryOperator)
  
and check_assign tenv e1 e2 = 
  let se1, _ = check_expr tenv e1 in
  let se2, _ = check_expr tenv e2 in
  let t1 = get_sexpr_type se1 in
  let t2 = get_sexpr_type se2 in
  if t1 = t2 (* assignment types must be exactly the same *)
     then S.SAssign(se1, se2, t1)
     else raise(E.AssignmentTypeMismatch(A.string_of_typ t1, A.string_of_typ t2))

and check_call tenv e args =
 let se, _ = check_expr tenv e in
    let str = A.string_of_expr e in
    let r = Str.regexp "\\([A-Za-z_]+\\)" in    (* in currying examples, parser may think the called function name in pokemonMotto("Ash")("!") is pokemonMotto("Ash") --> parse to pokemonMotto and then check the function *)
    let num = try Str.search_forward r str 0 with
      | Not_found -> raise(E.UndefinedFunction(str)) in
    let name = Str.matched_string str in
    let result = check_call_wrapper tenv name in
    if result = "A.Void" 
    then begin
      let res = check_call_fun tenv name in
      if res = "A.Void" then ( 
        let first_word = try Str.first_chars (A.string_of_expr e) 8 with 
	| Invalid_argument(err) -> A.string_of_expr e in
          if first_word <> "function" then ( (* check for anonymous funs *)
            let t = check_call_fun_vdecls tenv name in
            if t = A.Void then raise(E.UndefinedFunction(name))
            else (
              let get_s (s,_) = s in 
              let sargs = List.map (fun x -> get_s (check_expr tenv x)) args in
              S.SCallExpr(se, sargs, tenv.scope.return_type)
              ))
          else 
            let get_s (s,_) = s in 
            let sargs = List.map (fun x -> get_s (check_expr tenv x)) args in
            S.SCallExpr(se, sargs, tenv.scope.return_type)
            )
      else (let get_s (s,_) = s in 
          let sargs = List.map (fun x -> get_s (check_expr tenv x)) args in
          S.SCallExpr(se, sargs, tenv.scope.return_type))
    end
    else 
    let get_s (s,_) = s in 
    let sargs = List.map (fun x -> get_s (check_expr tenv x)) args in
    S.SCallExpr(se, sargs, tenv.scope.return_type)
   

(********************
 * Check Expressions
 ********************)
and check_expr tenv = function
	  A.IntLit i		-> S.SIntLit(i), tenv
	| A.BoolLit b		-> S.SBoolLit(b), tenv
	| A.FloatLit f		-> S.SFloatLit(f), tenv
	| A.CharLit c 		-> S.SCharLit(c), tenv
	| A.StringLit s		-> S.SStringLit(s), tenv
	| A.FunExpr f		-> check_fexpr tenv f, tenv
	| A.Id i		-> S.SId(i, get_id_type tenv i), tenv
	| A.Binop(e1,op,e2)	-> check_binop tenv e1 op e2, tenv
	| A.Unop(uop, e)	-> check_unop tenv uop e, tenv
	| A.Assign(e1, e2)	-> check_assign tenv e1 e2, tenv
	| A.Noexpr		-> S.SNoexpr, tenv
	| A.CallExpr(e, args)	-> check_call tenv e args, tenv 	

(* for check_expr, add in check_call *)
and get_sexpr_type = function 
	  S.SIntLit(_)		-> A.Int
	| S.SBoolLit(_)		-> A.Bool
	| S.SFloatLit(_)	-> A.Float
	| S.SCharLit(_)		-> A.Char
	| S.SStringLit(_)	-> A.String
	| S.SFunExpr(_)		-> A.Fun
	| S.SId(_, t)		-> t
	| S.SBinop(_,_,_,t)	-> t
	| S.SUnop(_,_,t)	-> t
	| S.SAssign(_,_,t)	-> t
	| S.SCallExpr(_,_,t)	-> t
	| S.SNoexpr		-> A.Void

(***********************
 * Check Statements
 ***********************)

(* check exprstmt by just checking expr *)
and check_expr_stmt tenv e =
  let sexpr, tenv = check_expr tenv e in
    S.SExprStmt(sexpr), tenv

(* check vdecl dup, vdecl type, and then add to symbol table *)
and check_vdecl tenv v =
  let get_v_expr (_,_, e) = e in (* helper to get expr from vdecl tuple *)
  let vexpr = get_v_expr v in
  let vsexpr, _ = check_expr tenv vexpr in
  let vstyp = get_sexpr_type vsexpr in
  let get_v_typ (t,_,_) = t in  (* helper to get typ of vdecl *)
  let vtyp = get_v_typ v in
  let get_v_name (_,n,_) = n in
  let vname = get_v_name v in
  if (vtyp = vstyp || vstyp = A.Void) (* check declared type of vdecl with its actual type, or if vdecl hasn't been initialized yet *) 
  then (tenv.scope.variables <- v:: tenv.scope.variables; 
    S.SVarDecl(vstyp, vname, vsexpr), tenv)  (* add the vdecl to symbol table *)
  else raise(E.VariableDeclarationTypeMismatch(vname)) 
  and check_vdecl_st tenv v =  
    let get_v_name (_,n,_) = n in
    let vname = get_v_name v in
      try List.find (fun x->(get_v_name x)=vname) tenv.scope.variables (* check to see if local variable already exists *)
      with | Not_found	-> v
           |  _ 	-> raise(E.DuplicateLocal(vname))
               
and check_fdecl tenv f =
  try find_fdecl_name tenv.scope f.A.fdFname  (* check name dups *)
  with | Not_found -> 
  let get_bind_string (_,s) = s in
  let formals_map = List.fold_left (fun m formal ->  (* check formal dups within current function *)
    if StringMap.mem (get_bind_string formal) m 
    then raise(E.DuplicateFormal(get_bind_string formal))
    else StringMap.add (get_bind_string formal) formal m) StringMap.empty f.A.fdFormals in  
  tenv.scope.fun_names <- tenv.scope.fun_names@[f.A.fdFname];
  match tenv.scope.parent with 
    | Some(parent) -> (parent.fun_names <- parent.fun_names@[f.A.fdFname]); 
  let scope' = (* create a new scope *)
    	{ 
	parent = Some(tenv.scope); (* parent may or may not exist *)
	variables = [(A.Fun, "print", A.Id("print"))]; 
	name = f.A.fdFname;
	return_type = f.A.fdReturnType;
	formals = f.A.fdFormals;
	fun_names = [f.A.fdFname];
	} in
  let tenv' = 
	{ tenv with scope = scope'; } in
  let get_ssl (sstmt_list, _) = sstmt_list in 
    let sslp = check_stmt_list tenv' f.A.fdBody in scope'.variables <- List.rev scope'.variables;
    let sfdecl = {
      S.sfdReturnType = f.A.fdReturnType;
      S.sfdFname = f.A.fdFname;
      S.sfdFormals = f.A.fdFormals;
      S.sfdBody = get_ssl sslp;
    } in 
      S.SFunDecl(sfdecl), tenv (* return the original tenv after checking the fdecl *)
  
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
  let restore = tenv.in_for in
    let tenv = update_env_context tenv true tenv.in_while in (* in for loop *)
    let se1, _ = check_expr tenv e1 in
    let se2, _ = check_expr tenv e2 in
    let se3, _ = check_expr tenv e3 in
    let forblock, _ = check_stmt tenv s in
    let typ = get_sexpr_type se2 in
    let result =  (* condition can be boolean or nothing, e.g. for(;;) *)
      if (typ = A.Bool || typ = A.Void) then S.SFor(se1, se2, se3, forblock)
      else raise(E.InvalidForStatementCondition) in
    let tenv = update_env_context tenv restore tenv.in_while (* out of for loop*)
    in result, tenv 

and check_while tenv e s =
  let restore = tenv.in_while in
    let tenv = update_env_context tenv tenv.in_for true in
    let se, _ = check_expr tenv e in
    let typ = get_sexpr_type se in
    let whileblock, _ = check_stmt tenv s in
    let result = 
      if (typ = A.Bool || typ = A.Void) then S.SWhile(se, whileblock)
      else raise(E.InvalidWhileStatementCondition) in
    let tenv = update_env_context tenv tenv.in_for restore
    in result, tenv
 
and check_stmt tenv = function
	  A.Block sl		-> check_block tenv sl
	| A.ExprStmt e		-> check_expr_stmt tenv e
	| A.VarDecl v		-> check_vdecl_st tenv v; check_vdecl tenv v  
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
 * Program entry point
 ***********************)
let root_symbol_table : symbol_table = {
  parent	 = None;
  name		 = "anon"; 
  variables	 = [(A.Fun, "print", A.Id("print"))]; (* built-in function print *)
  return_type	 = A.Void;
  formals	 = []; 
  fun_names 	 = [];
}

let print_symbol_table : symbol_table = {
  parent	 = Some(root_symbol_table);
  name		 = "print"; 
  variables	 = [(A.Fun, "print", A.Id("print"))];
  return_type	 = A.Void;
  formals	 = [(A.String, "input")];
  fun_names	 = [];
}

let root_env : translation_env = {
  scope = print_symbol_table;
  in_for = false;
  in_while = false;
}

let check_ast ast = match ast with
	  A.Program(stmts) -> let (sast, _) = check_stmt_list root_env stmts in sast
	| _ -> raise(E.InvalidCompilerArgument)

(* Testing *) 
let test_ok (sast : S.sstmt list) = match sast with  (* with MatchaScript.ml *)
  _ -> "okay\n"
