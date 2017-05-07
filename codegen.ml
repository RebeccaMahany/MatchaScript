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
  parent: symbol_table option; (* option means a parent scope is optional *)
  mutable st_vdecls: svdecl list;
  mutable st_fdecls: sfdecl list;
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

let st_toplevel : symbol_table = {
  parent =  None;
  st_vdecls = [];
  st_fdecls = [];
}

let context = L.global_context ()
let the_module = L.create_module context "MatchaScript"
let builder     = L.builder context
let i32_t  = L.i32_type       context
let i8_t   = L.i8_type        context
let i1_t   = L.i1_type        context
let fl_t   = L.float_type     context
let str_t  = L.pointer_type (L.i8_type context)
let void_t = L.void_type      context
let st:symbol_table = st_toplevel

let codegen_main sstmt_list builder = 


let translate (sstmt_list : sstmt list) =
  (* Add builtins *)
  (* Forward-declare the program functions *)
  (* Forward-declare the structs for the program functions 
    (using the symbol tables) *)
  (* Generate LLVM IR for the structs of the program functions *)
  (* Generate LLVM IR for functions *)
  (* Generate LLVM IR for main function *)
  let _ = codegen_main sstmt_list builder in
  the_module