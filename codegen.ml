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
    A.IntLit _ -> A.Int
  | A.FloatLit _ -> A.Float
  | A.BoolLit _ -> A.Bool
  | A.CharLit _ -> A.Char
  | A.StringLit _ -> A.String 

(* Builtins *)
(* printf *)
let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |]
let printf_func = L.declare_function "printf" printf_t the_module

(*************************
Forward Declarations
*************************)
let codegen_func_fwd_decls (sast : sstmt list) =
  let gen_func_fwd_decl sstmt = match sstmt with
    SFunDecl(f) -> 
      let name = f.sfdFname
      and formal_types = Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) f.sfdFormals) in 
      let ftype = L.function_type (ltype_of_typ f.sfdReturnType) formal_types in
      (* Adds the pair (name, (LLVM fwd_declaration, ocaml_sfdecl)) to the StringMap m *)
      Hashtbl.add fwd_decls_hashtbl name (L.define_function name ftype the_module, f)
  in List.iter gen_func_fwd_decl sast
;;

(*************************
Function Definitions
*************************)
let build_function_body f =
  (* Expressions *)
  (*
  let codegen_sexpr llbuilder = function
      SIntLit i -> L.const_int i32_t i
    | SFloatLit f -> L.const_float fl_t f
    | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | SCharLit c -> L.const_int i8_t c
    | SStringLit s -> L.build_global_stringptr s "string" llbuilder
    | SNoexpr -> L.const_int i32_t 0
    | SId s -> L.build_load (lookup s) s llbuilder
    | SBinop (e1, op, e2) ->
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
    | SUnop(op, e) ->
        let e' = codegen_sexpr llbuilder e in
        (match op with
            A.Neg     -> L.build_neg
          | A.Not     -> L.build_not) e' "tmp" llbuilder
    | SAssign (s, e) -> let e' = codegen_sexpr llbuilder e in
        ignore (L.build_store e' (var_lookup s) llbuilder); e'
    | SCall ("print", [e]) -> 
        let codegen_call_printf e llbuilder = 
          let int_format_str llbuilder = L.build_global_stringptr "%d\n" "fmt" llbuilder
          and float_format_str llbuilder = L.build_global_stringptr "%f\n" "fmt" llbuilder
          and char_format_str llbuilder = L.build_global_stringptr "%c\n" "fmt" llbuilder 
          and str_format_str llbuilder = L.build_global_stringptr "%s\n" "fmt" llbuilder in

          let format_str e_typ llbuilder = match e_typ with
              A.Int -> int_format_str llbuilder
            | A.Float -> float_format_str llbuilder
            | A.Char -> char_format_str llbuilder
            | A.String -> str_format_str llbuilder
            | _ -> raise (Failure "Invalid printf type")
          in
          let e' = codegen_scodegen_sexpr llbuilder e
          and e_type = gen_type e in
          L.build_call printf_func [| format_str e_typ llbuilder; e' |]
            "printf" llbuilder
        in codegen_call_printf e llbuilder
    | SCall (f, act) ->
        let (fdef, sfdecl) = StringMap.find f function_decls in
        let actuals = List.rev (List.map (codegen_sexpr llbuilder) (List.rev act)) in
        let result = (match sfdecl.A.typ with A.Void -> ""
                                                | _ -> f ^ "_result") in
        L.build_call fdef (Array.of_list actuals) result llbuilder

  (* Statements *)
  and codegen_sstmt llbuilder = function
      SBlock sl -> List.fold_left sstmt llbuilder sl
    | SExpr e -> ignore (codegen_sexpr llbuilder e); llbuilder
    | SReturn e -> ignore (match sfdecl.sfdReturnType with
                                A.Void -> L.build_ret_void llbuilder
                              | _      -> L.build_ret (codegen_scodegen_sexpr llbuilder e) llbuilder); llbuilder
    | SIf (predicate, then_stmt, else_stmt) ->
        let bool_val = codegen_sexpr llbuilder predicate in
        let merge_bb = L.append_block context "merge" the_function in
        let then_bb = L.append_block context "then" the_function in
        add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
        (L.build_br merge_bb);
        let else_bb = L.append_block context "else" the_function in
        add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
        (L.build_br merge_bb);
        ignore (L.build_cond_br bool_val then_bb else_bb llbuilder);
        L.builder_at_end context merge_bb
    | SWhile (predicate, body) ->
        let pred_bb = L.append_block context "while" the_function in
        ignore (L.build_br pred_bb llbuilder);
        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (stmt (L.builder_at_end context body_bb) body)
        (L.build_br pred_bb);
        let pred_builder = L.builder_at_end context pred_bb in
        let bool_val = expr pred_builder predicate in
        let merge_bb = L.append_block context "merge" the_function in
        ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
        L.builder_at_end context merge_bb
    | SFor (e1, e2, e3, body) -> sstmt llbuilder
        ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
  in
  (* Find function in hashtbl of function forward declarations *)
  let (the_function, _) = StringMap.find sfdecl.sfdFname function_decls in
  let llbuilder = L.builder_at_end context (L.entry_block the_function) *)
  ()
;;

let codegen_func_defs (sast : sstmt list) =
  let gen_func_def sstmt = match sstmt with
    SFunDecl(f) -> build_function_body f
  in List.iter gen_func_def sast
;;

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
  prog_main

  (* forward declare main function *)

  (* build_function_body main function *)
;;

(*************************
Program entry point
*************************)
let translate (sast : sstmt list) =
  let _ = codegen_func_fwd_decls sast in
  let _ = codegen_func_defs sast in
  let prog_main = codegen_build_main sast in
  let _ = build_function_body prog_main in
  the_module
;;