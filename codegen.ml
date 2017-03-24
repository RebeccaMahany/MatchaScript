(* 
MatchaScript codegen.ml
*)

(* Code generation: translate takes a semantically checked AST and
produces LLVM IR
LLVM tutorial: Make sure to read the OCaml version of the tutorial
http://llvm.org/docs/tutorial/index.html
Detailed documentation on the OCaml LLVM library:
http://llvm.moe/
http://llvm.moe/ocaml/
*)

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (globals, _, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "MatchaScript"
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and str_t  = L.pointer_type (L.i8_type context)
  and void_t = L.void_type context in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in    

  let int_format_str b = L.build_global_stringptr "%d\n" "fmt" b
  and str_format_str b = L.build_global_stringptr "%s\n" "fmt" b in

  (* Global variables *)
  let global_vars = ref StringMap.empty in 

  (* Current function and local variables *)
  let local_vars = ref StringMap.empty in
  let currentf = ref (List.hd functions) in (* List.hd = list head *)

  (* Return the value or the type for a variable or formal argument *)
  (* All the tables have the structure (type, llvalue) *)
  let name_to_llval n : L.llvalue = 
    try (snd (StringMap.find n !local_vars))
    with Not_found -> (snd (StringMap.find n !global_vars))
  in

  let name_to_type n : A.typ =
    try (fst (StringMap.find n !local_vars))
    with Not_found -> (fst (StringMap.find n !global_vars)) in

  (* MS to LLVM Type Conversions*)
  let rec ltype_of_typ = function
      A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Void -> void_t 
    | A.Str -> str_t 

  (* Get type of expr *)
  and ms_typeof = function
  		A.IntLit _ 	-> A.Int
  	|	A.BoolLit _ -> A.Bool
  	|	A.StrLit _ 	-> A.Str
  	| A.Id s 			-> (match (name_to_type s) with
  											_ as ty -> ty)
  	| A.Call(s, _) -> let fdecl =
  											List.find (fun x -> x.A.fname = s) functions in
                              (match fdecl.A.typ with
                                _ as ty -> ty)
    | A.Binop(e1, _, _) -> ms_typeof e1
    | A.Unop(_, e1)			-> ms_typeof e1
    | A.Assign(s, _)		-> ms_typeof (A.Id(s))
    | A.Noexpr          -> raise (Failure "corrupted tree - Noexpr as a statement")

  and lreturn_type_ty = match ty with
  	| _ -> ltype_of_typ ty

  and format_str x_type builder =
  	let b = builder in
  		match x_type with
  			A.Int -> int_format_str
			| A.Str -> str_format_str
			| _ -> raise (Failure "Invalid printf type")
	in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname
      and formal_types = Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions 
  in
  
  (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
  let rec add_terminal builder f =
    match L.block_terminator (L.insertion_block builder) with
  		Some _ -> ()
    | None -> ignore (f builder) 

  (* Return the code of an expression to be built in LLVM *)
  and expr builder = let b = builder in function 
    	A.IntLit i -> L.const_int i32_t i
  	| A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
  	| A.StrLit sl -> L.build_global_stringptr sl "string" b
    | A.Noexpr -> L.const_int i32_t 0
    | A.Id s -> L.build_load (name_to_llval s) s builder
    | A.Binop (e1, op, e2) ->
    		let e1' = expr b e1
  		  and e2' = expr b e2 in
    		(match op with
		      A.Add     -> L.build_add
		    | A.Sub     -> L.build_sub
		    | A.Mult    -> L.build_mul
		    | A.Div     -> L.build_sdiv
		    | A.And     -> L.build_and
		    | A.Or      -> L.build_or
		    | A.Equal   -> L.build_icmp L.Icmp.Eq
		    | A.Neq     -> L.build_icmp L.Icmp.Ne
		    | A.Less    -> L.build_icmp L.Icmp.Slt
		    | A.Leq     -> L.build_icmp L.Icmp.Sle
		    | A.Greater -> L.build_icmp L.Icmp.Sgt
		    | A.Geq     -> L.build_icmp L.Icmp.Sge
    		) e1' e2' "tmp" b
  	| A.Unop(op, e) ->
		    let e' = expr b e in
		    (match op with
		      A.Neg     -> L.build_neg
		    | A.Not     -> L.build_not) e' "tmp" b
		| A.Assign (s, e) -> let e' = expr b e in
		   ignore (L.build_store e' (name_to_llval s) b); e'
  	| A.Call ("print", [e]) | A.Call ("printb", [e]) ->
			L.build_call printf_func [| (format_str (ms_typeof e) b) ; (expr b e) |]
      	"printf" b
    | A.Call (f, act) ->
       let (fdef, fdecl) = StringMap.find f function_decls in
		   let actuals = List.rev (List.map (expr b) (List.rev act)) in
		   let result = (match fdecl.A.typ with A.Void -> ""
		                                            | _ -> f ^ "_result") in
    L.build_call fdef (Array.of_list actuals) result b
  
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
  and stmt builder = let (the_function, _) = StringMap.find !currentf.A.fname function_decls in
  	function
  			A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Return e -> ignore (match fdecl.A.typ with
    			A.Void -> L.build_ret_void builder
  			| _ -> L.build_ret (expr builder e) builder); builder
      | A.If (predicate, then_stmt, else_stmt) ->
        	let bool_val = expr builder predicate in
		   		let merge_bb = L.append_block context "merge" the_function in
		   		
		   		(* Emit 'then' value *)
		   		let then_bb = L.append_block context "then" the_function in
		   		add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
		     	(L.build_br merge_bb);

		     	(* Emit 'else' value *)
		     	let else_bb = L.append_block context "else" the_function in
		     	add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
		     	(L.build_br merge_bb);
		     	
		     	(* Add the conditional branch *)
   				ignore (L.build_cond_br bool_val then_bb else_bb builder);
   				L.builder_at_end context merge_bb
      | A.While (predicate, body) ->
    			let pred_bb = L.append_block context "while" the_function in
    				ignore (L.build_br pred_bb builder);
			    let body_bb = L.append_block context "while_body" the_function in
  	 				add_terminal (stmt (L.builder_at_end context body_bb) body)
    		  	(L.build_br pred_bb);
    			let pred_builder = L.builder_at_end context pred_bb in
    			let bool_val = expr pred_builder predicate in
    			let merge_bb = L.append_block context "merge" the_function in
    			ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
    			L.builder_at_end context merge_bb
      | A.For (e1, e2, e3, body) -> stmt builder
      		( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
    	| A.Nostmt -> ignore (0); builder
  in

  (* Build global StringMap *)
  and global_var m (t, n, e) =
    let (f,_) = StringMap.find "main" function_decls in
    let builder = L.builder_at_end context (L.entry_block f) in
    (* Build the first initialization of the variables *)
    let rec init t e = match e with
          A.IntLit _ | A.FloatLit _ | A.BoolLit _ | A.StringLit _ -> expr builder e
        | A.TupleLit(_) -> 
            let ty     = ltype_of_typ t in
            L.const_ptrtoint (L.const_int i32_t 0) ty
        | _ -> 
              match t with
              A.Int   -> expr builder (A.IntLit(0))
            | A.Float -> expr builder (A.FloatLit(0.0))
            | A.String -> expr builder (A.StringLit(""))
            | A.Bool   -> expr builder (A.BoolLit(true))
            | A.Tuple(_, _) -> (init t (A.TupleLit([])))
            | A.None -> expr builder (A.Noexpr)
    in
    let tuple = (t, (L.define_global n (init t e) the_module)) in
    StringMap.add n tuple m 


    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module