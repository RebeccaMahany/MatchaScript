(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

open Sast

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let context = L.global_context ()
let the_module = L.create_module context "MatchaScript"
let llbuilder = L.builder       context
let i32_t  = L.i32_type       context
let i8_t   = L.i8_type        context
let i1_t   = L.i1_type        context
let fl_t   = L.float_type     context
let str_t  = L.pointer_type (L.i8_type context)
let void_t = L.void_type      context

let fwd_decls_hashtbl : (string, (L.llvalue * sfdecl)) Hashtbl.t = Hashtbl.create 10

let ltype_of_typ = function
    A.Int -> i32_t
  | A.Float -> fl_t
  | A.Bool -> i1_t
  | A.Char -> i8_t
  | A.Void -> void_t
  | A.String -> str_t
    
let gen_type = function
    SIntLit _ -> A.Int
  | SFloatLit _ -> A.Float
  | SBoolLit _ -> A.Bool
  | SCharLit _ -> A.Char
  | SStringLit _ -> A.String 

(* Builtins *)
(* printf *)
let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |]
let printf_func = L.declare_function "printf" printf_t the_module

(*************************
Forward Declarations
*************************)
let gen_func_fwd_decl (f : sfdecl) = 
  let name = f.sfdFname
  and formal_types = Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) f.sfdFormals) in 
  let ftype = L.function_type (ltype_of_typ f.sfdReturnType) formal_types in
  (* Adds the pair (name, (LLVM fwd_declaration, ocaml_sfdecl)) to the StringMap m *)
  Hashtbl.add fwd_decls_hashtbl name (L.define_function name ftype the_module, f)

let codegen_func_fwd_decls (sast : sstmt list) =
  let get_fdecls_for_fwd_decl_generation sstmt = match sstmt with
    SFunDecl(f) -> gen_func_fwd_decl f
  in List.iter get_fdecls_for_fwd_decl_generation sast

(*************************
Function Definitions
*************************)
let build_function_body f_build =
  let (the_function, _) = Hashtbl.find fwd_decls_hashtbl f_build.sfdFname in
  let llbuilder = L.builder_at_end context (L.entry_block the_function) in

  (* Construct the function's "locals": formal arguments and locally declared variables. 
    Allocate each on the stack, initialize their value, if appropriate, and remember their
    values in the "locals" map *)
  let local_vars =
    let add_formal m (t, n) p = L.set_value_name n p;
      let local = L.build_alloca (ltype_of_typ t) n llbuilder in
      ignore (L.build_store p local llbuilder);
      StringMap.add n local m 
    in

    let add_local m (t, n) =
      let local_var = L.build_alloca (ltype_of_typ t) n llbuilder in 
      StringMap.add n local_var m
    in

    let f_formals = List.fold_left2 add_formal StringMap.empty f_build.sfdFormals
      (Array.to_list (L.params the_function)) in

    let f_locals = 
      let extract_locals_from_fbody fbody = 
        let handle_vdecl locals_list stmt = match stmt with
          SVarDecl(typ, id, expr) -> (typ, id) :: locals_list
        in
        List.fold_left handle_vdecl [] fbody  (* fbody is a stmt list*)
      in extract_locals_from_fbody f_build.sfdBody 
    in
    (* extract locals from the stmt list of the function *)
    List.fold_left add_local f_formals f_locals 
  in

  (* Return the value for a variable or formal argument *)
  let var_lookup n = try StringMap.find n local_vars
                    with Not_found -> raise (Failure "Variable not found")
  in

  (* Expressions *)
  let rec codegen_sexpr llbuilder = function
      SIntLit i -> L.const_int i32_t i
    | SFloatLit f -> L.const_float fl_t f
    | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
(*    | SCharLit c -> L.const_int i8_t c *)
    | SStringLit s -> L.build_global_stringptr s "string" llbuilder
    | SNoexpr -> L.const_int i32_t 0
    | SId(s, typ) -> L.build_load (var_lookup s) s llbuilder
    | SBinop (e1, op, e2, typ) ->
        let e1' = codegen_sexpr llbuilder e1
        and e2' = codegen_sexpr llbuilder e2 in
        (match op with
          A.Add     -> L.build_add
        | A.Sub     -> L.build_sub
        | A.Mult    -> L.build_mul
        | A.Div     -> L.build_sdiv
        | A.Mod     -> L.build_srem
        | A.And     -> L.build_and
        | A.Or      -> L.build_or
        | A.Equal   -> L.build_icmp L.Icmp.Eq
        | A.Neq     -> L.build_icmp L.Icmp.Ne
        | A.Less    -> L.build_icmp L.Icmp.Slt
        | A.Leq     -> L.build_icmp L.Icmp.Sle
        | A.Greater -> L.build_icmp L.Icmp.Sgt
        | A.Geq     -> L.build_icmp L.Icmp.Sge
        ) e1' e2' "tmp" llbuilder
    | SUnop(op, e, typ) ->
        let e' = codegen_sexpr llbuilder e in
        (match op with
          A.Neg     -> L.build_neg
        | A.Not     -> L.build_not) e' "tmp" llbuilder
    | SAssign (SId(s,_), e, t) -> let e' = codegen_sexpr llbuilder e in
        ignore (L.build_store e' (var_lookup s) llbuilder); e'
    | SCallExpr (SId("print", _), [e], typ) -> 
        let int_format_str llbuilder = L.build_global_stringptr "%d\n" "fmt" llbuilder;
        and float_format_str llbuilder = L.build_global_stringptr "%f\n" "fmt" llbuilder;
        and char_format_str llbuilder = L.build_global_stringptr "%c\n" "fmt" llbuilder;
        and str_format_str llbuilder = L.build_global_stringptr "%s\n" "fmt" llbuilder in          
          
        let format_str e_typ llbuilder = match e_typ with
            A.Int -> int_format_str llbuilder
          | A.Float -> float_format_str llbuilder
          | A.Char -> char_format_str llbuilder
          | A.String -> str_format_str llbuilder
          | _ -> raise (Failure "Invalid printf type")
        in

        let e' = codegen_sexpr llbuilder e
        and e_type = gen_type e in
        L.build_call printf_func [| format_str e_type llbuilder; e' |]
            "printf" llbuilder
    | SCallExpr (SId(f, _), act, typ) ->
        let (fdef, f_build) = Hashtbl.find fwd_decls_hashtbl f in
        let actuals = List.rev (List.map (codegen_sexpr llbuilder) (List.rev act)) in
        let result = (match f_build.sfdReturnType with A.Void -> ""
                                                      | _     -> f ^ "_result") in
        L.build_call fdef (Array.of_list actuals) result llbuilder
      in

      let add_terminal llbuilder f =
      match L.block_terminator (L.insertion_block llbuilder) with
        Some _ -> ()
      | None -> ignore (f llbuilder) in
  
  (* Statements *)
  let rec codegen_sstmt llbuilder = function
      SBlock sl -> List.fold_left codegen_sstmt llbuilder sl
    | SExprStmt e -> ignore (codegen_sexpr llbuilder e); llbuilder
    | SReturn e -> ignore (match f_build.sfdReturnType with
                                A.Void -> L.build_ret_void llbuilder
                              | _      -> L.build_ret (codegen_sexpr llbuilder e) llbuilder); llbuilder
    | SVarDecl (typ, id, sexpr) -> (* Assign the declared variable *)
        let e' = codegen_sexpr llbuilder sexpr in
        ignore (L.build_store e' (var_lookup id) llbuilder); llbuilder
    | SIf (predicate, then_stmt, else_stmt) ->
        let bool_val = codegen_sexpr llbuilder predicate in
        let merge_bb = L.append_block context "merge" the_function in
        let then_bb = L.append_block context "then" the_function in
        add_terminal (codegen_sstmt (L.builder_at_end context then_bb) then_stmt)
        (L.build_br merge_bb);
        let else_bb = L.append_block context "else" the_function in
        add_terminal (codegen_sstmt (L.builder_at_end context else_bb) else_stmt)
        (L.build_br merge_bb);
        ignore (L.build_cond_br bool_val then_bb else_bb llbuilder);
        L.builder_at_end context merge_bb
    | SWhile (predicate, body) ->
        let pred_bb = L.append_block context "while" the_function in
        ignore (L.build_br pred_bb llbuilder);
        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (codegen_sstmt (L.builder_at_end context body_bb) body)
        (L.build_br pred_bb);
        let pred_builder = L.builder_at_end context pred_bb in
        let bool_val = codegen_sexpr pred_builder predicate in
        let merge_bb = L.append_block context "merge" the_function in
        ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
        L.builder_at_end context merge_bb
    | SFor (e1, e2, e3, body) -> codegen_sstmt llbuilder
        ( SBlock [SExprStmt e1 ; SWhile (e2, SBlock [body ; SExprStmt e3]) ] )
  in
  
  (* Build the code for each statement in the function *)
  let llbuilder = codegen_sstmt llbuilder (SBlock f_build.sfdBody) in
  
  (* Add a return if the last block falls off the end *)
  add_terminal llbuilder (match f_build.sfdReturnType with
      A.Void -> L.build_ret_void
    | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
;;

let codegen_func_defs (sast : sstmt list) =
  let gen_func_def sstmt = match sstmt with
    SFunDecl(f) -> build_function_body f
  in List.iter gen_func_def sast

(*************************
Main
*************************)
let codegen_build_main (sast : sstmt list) =
  let wrap_sstmt_list (sl : sstmt list) =
    let sfdecl_main : sfdecl = {
      sfdReturnType = A.Int;
      sfdFname = "main";
      sfdFormals = [];
      (* Append a return statement to the body *)
      sfdBody = sl@[SReturn(SIntLit(0))];
      }
    in 
    sfdecl_main;
  in
  let prog_main = wrap_sstmt_list sast in

  (* forward declare main function *)
  let _ = gen_func_fwd_decl prog_main in

  (* build_function_body main function *)
  let _ = build_function_body prog_main in
  ()

(*************************
Program entry point
*************************)
let translate (sast : sstmt list) =
  let _ = codegen_func_fwd_decls sast in
  let _ = codegen_func_defs sast in
  let _ = codegen_build_main sast in
  the_module
;;