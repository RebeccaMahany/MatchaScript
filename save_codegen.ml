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


type symbol_table = {
  name: string;
  parent: symbol_table option; (* option means a parent scope is optional *)
  mutable st_formals: svdecl list;
  mutable st_vdecls: svdecl list;
  mutable st_fdecls: sfdecl list;
  children: symbol_table list;

let rec find_svdecl (scope : symbol_table) name =
  try
    (* a svdecl is (typ, string, expr) *)
    List.find (fun (_, s, _) -> s = name) scope.st_vdecls 
  with Not_found ->
    match scope.parent with
      Some(parent) -> find_svdecl parent name
    | _ -> raise Not_found

let rec find_sfdecl (scope : symbol_table) name =
  try
    List.find (fun (sfdecl : sfdecl) -> sfdecl.sfdFname = name) scope.st_fdecls 
  with Not_found ->
    match scope.parent with
      Some(parent) -> find_sfdecl parent name
    | _ -> raise Not_found

let context = L.global_context ()
let the_module = L.create_module context "MatchaScript"
let llbuilder = L.builder       context
let i32_t  = L.i32_type       context
let i8_t   = L.i8_type        context
let i1_t   = L.i1_type        context
let fl_t   = L.float_type     context
let str_t  = L.pointer_type (L.i8_type context)
let void_t = L.void_type      context

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
let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
let printf_func = L.declare_function "printf" printf_t the_module

(* call printf *)
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
  let e' = expr llbuilder e
  and e_type = gen_type e in
  L.build_call printf_func [| format_str e_typ llbuilder; e' |]
    "printf" llbuilder

let wrap_sstmt_list (sl : sstmt list) =
  let sfdecl_main : sfdecl = {
    sfdReturnType = A.Int;
    sfdFname = "main";
    sfdFormals = [];
    (* Append a return statement to the body *)
    sfdBody = sl@[SReturn(SIntLit(0))];
  } in
  sfdecl_main

let build_function_body fdecl =

(*************************
Expressions
*************************)
and codegen_sexpr llbuilder = function
  A.IntLit i -> L.const_int i32_t i
  | A.FloatLit f -> L.const_float fl_t f
  | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
  | A.CharLit c -> L.const_int i8_t c
  | A.StringLit s -> L.build_global_stringptr s "string" builder
  | A.Noexpr -> L.const_int i32_t 0
  | A.Id s -> L.build_load (lookup s) s builder
  | A.Binop (e1, op, e2) ->
      let e1' = expr builder e1
      and e2' = expr builder e2 in
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
          ) e1' e2' "tmp" builder
  | A.Unop(op, e) ->
      let e' = expr builder e in
      (match op with
          A.Neg     -> L.build_neg
        | A.Not     -> L.build_not) e' "tmp" builder
  | A.Assign (s, e) -> let e' = expr builder e in
      ignore (L.build_store e' (lookup s) builder); e'
  | A.Call ("print", [e]) -> codegen_call_printf llbuilder
  | A.Call (f, act) ->
      let (fdef, fdecl) = StringMap.find f function_decls in

and codegen_call llbuilder d el = function
      "print"   -> codegen_print el llbuilder
  |  _ as fname  -> raise 

(*************************
Statements
*************************)
and codegen_stmt llbuilder = function
    SBlock sl             -> List.hd(List.map (codegen_stmt llbuilder) sl)
  |   SExpr(e, d)             -> codegen_sexpr llbuilder e
  |   SReturn(e, d)         -> codegen_ret d e llbuilder
  |   SIf (e, s1, s2)           -> codegen_if_stmt e s1 s2 llbuilder
  |   SFor (e1, e2, e3, s)      -> codegen_for e1 e2 e3 s llbuilder
  |   SWhile (e, s)         -> codegen_while e s llbuilder
  |   SBreak                -> codegen_break llbuilder   
  |   SContinue             -> codegen_continue llbuilder
  |   SLocal(d, s, e)       -> codegen_alloca d s e llbuilder

let translate (sstmt_list : sstmt list) =
  (* wrap the incoming sstmt_list in a main sfdecl *)
  let (prog_main : sfdecl) = wrap_sstmt_list sstmt_list in

  (* Generate LLVM IR for main function *)
  let _ = build_function_body prog_main in
  the_module