(* Top-level of the MatchaScript compiler: scan & parse the input,
   check the resulting AST, generate LLVM IR, and dump the module *)

type action = Ast | Sast | LLVM_IR | Compile

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);      (* Pretty-print the AST *)
                              ("-s", Sast);     (* Pretty-print the SAST *)
                              ("-l", LLVM_IR);  (* Generate LLVM, don't check *)
                              ("-c", Compile) ] (* Generate, check LLVM IR *)
  else Compile in
  let lexbuf = Lexing.from_channel stdin in
  let ast_nomain = Parser.program Scanner.token lexbuf in
  let ast = Top_ast.top_ast ast_nomain in
  let sast = Analyzer.check_ast ast (*Fake_sast.create_fake_sast*) in
  match action with
      Ast -> print_string (Ast.string_of_program ast)
    | Sast -> print_string (Analyzer.test_ok sast)
    | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate sast))
    | Compile -> let m = Codegen.translate sast in
        Llvm_analysis.assert_valid_module m;
        print_string (Llvm.string_of_llmodule m)
