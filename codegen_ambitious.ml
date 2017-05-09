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
}

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
let builder = L.builder context
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
(*
(* Builtins *)
(* printf *)
let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
let printf_func = L.declare_function "printf" printf_t the_module

(* call printf *)
let codegen_call_printf e builder = 
  let int_format_str builder = L.build_global_stringptr "%d\n" "fmt" builder
  and float_format_str builder = L.build_global_stringptr "%f\n" "fmt" builder
  and char_format_str builder = L.build_global_stringptr "%c\n" "fmt" builder 
  and str_format_str builder = L.build_global_stringptr "%s\n" "fmt" builder in

  let format_str e_typ builder = match e_typ with
      A.Int -> int_format_str builder
    | A.Float -> float_format_str builder
    | A.Char -> char_format_str builder
    | A.String -> str_format_str builder
    | _ -> raise (Failure "Invalid printf type")
  in
  let fun_llvalue 
  L.build_call 
*)

let wrap_sstmt_list (sl : sstmt list) =
  let sfdecl_main : sfdecl = {
    sfdReturnType = A.Int;
    sfdFname = "main";
    sfdFormals = [];
    (* Append a return statement to the body *)
    sfdBody = sl@[SReturn(SIntLit(0))];
  } in
  sfdecl_main

(* Forward-declare structs for the program functions and any returned function 
pointers - implementation *)
let codegen_FuncRec_stub s =
  let struct_t = L.named_struct_type context s.fname
  in
  Hashtbl.add struct_types

let codegen_Fp_stub s = 

let find_fdecls_fexprs_and_freturns_recursively main_fdecl = 
  

(* Forward-declare the program functions - implementation *)

(* Generate LLVM IR to define the structs of the program functions and returned 
function pointers - implementation *)

(* Generate LLVM IR for functions - implementation *)


(**************************************************************
      Generate LLVM IR for main function - implementation
 **************************************************************)
let sym_table : symbol_table = {
  name = "main";
  parent =  None;
  st_formals = [];
  st_vdecls = [];
  st_fdecls = [];
  children = [];
}

let codegen_main prog_main builder =


let translate (sstmt_list : sstmt list) =
  (* wrap the incoming sstmt_list in a main sfdecl *)
  let (prog_main : sfdecl) = wrap_sstmt_list sstmt_list in

  (* Forward-declare in LLVM the structs for the program functions and any returned functions
    (using the symbol tables) *)


  (* Forward-declare in LLVM the program functions (all sfdecls and sfexprs) *)

  (* Generate LLVM IR to define the structs of the program functions and returned function pointers *)

  (* Generate LLVM IR for functions *)

  (* Generate LLVM IR for main function *)
  let _ = codegen_main prog_main builder in
  the_module