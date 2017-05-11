open Ast

type sbind = typ * string

type svdecl = typ * string * sexpr

and sexpr = 
    SIntLit of int
	| SFloatLit of float
	| SBoolLit of bool
	| SCharLit of char
	| SStringLit of string
	| SFunExpr of sfexpr 
	| SId of string * typ
	| SBinop of sexpr * op * sexpr * typ
	| SUnop of uop * sexpr * typ
	| SAssign of sexpr * sexpr * typ
	| SCallExpr of sexpr * sexpr list * typ 
	| SNoexpr

and scaseType = SDefault | SCaseType of sexpr

and sstmt = 
    SBlock of sstmt list
  | SExprStmt of sexpr
  | SVarDecl of svdecl
  | SFunDecl of sfdecl
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SDoWhile of sstmt * sexpr
  | SSwitch of sexpr * scase list

and scase = {
    scase : scaseType;
    ssetStmt : sstmt list;
}

and sfexpr = {
  sfeReturnType : typ;
  sfeFormals : sbind list;
  sfeBody: sstmt list;
}

and sfdecl = {
  sfdReturnType : typ;
  sfdFname : string;
  sfdFormals : bind list;
  sfdBody : sstmt list;
}
(*
and scdecl = {
  scname : string;
  scproperties : svdecl list;
}
*)
and sprogram = sstmt list

(* Pretty-printing functions *)
let string_of_sbind (t, id) = string_of_typ t ^ " " ^ id

let rec string_of_sexpr = function
    SIntLit(i) -> string_of_int i
  | SFloatLit(f) -> string_of_float f
  | SBoolLit(b) -> if b then "true" else "false"
  | SCharLit(c) -> "\'" ^ String.make 1 c ^ "\'"
  | SStringLit(s) -> "\"" ^ s ^ "\""
  | SFunExpr(f) -> string_of_sfexpr f
  | SId(s,t) -> s ^ "#" ^ string_of_typ t ^ "#"
  | SBinop(e1, o, e2, t) ->
     "("^ string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2 ^")#"^string_of_typ t^"#"
  | SUnop(o, e, t) -> string_of_uop o ^ string_of_sexpr e ^"#"^string_of_typ t^"#"
  | SAssign(e1, e2, t) -> "("^string_of_sexpr e1 ^ " = " ^ string_of_sexpr e2 ^")#"^string_of_typ t^"#"
  | SCallExpr(call_expr, args, t) ->
      string_of_sexpr call_expr ^ "(" ^ String.concat ", " (List.map string_of_sexpr args) ^ ")" ^"#"^string_of_typ t^"#"
  | SNoexpr -> ""

and string_of_sstmt = function
    SBlock(sstmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt sstmts) ^ "}\n"
  | SExprStmt(sexpr) -> string_of_sexpr sexpr ^ ";\n"
  | SVarDecl(v) -> string_of_svdecl v
  | SFunDecl(fd) -> string_of_sfdecl fd
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n";
  | SIf(e, s, SBlock([])) -> "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s  

and string_of_svdecl (typ, str, sexpr) = 
  if sexpr = SNoexpr then string_of_typ typ ^ " " ^ str ^ ";\n"
  else string_of_typ typ ^ " " ^ str ^ " = " ^ string_of_sexpr sexpr ^ ";\n"

and string_of_sfexpr sfexpr =
  "function " ^ string_of_typ sfexpr.sfeReturnType ^ " " 
  ^ "(" ^ String.concat ", " (List.map string_of_sbind sfexpr.sfeFormals) ^
  ")\n{\n" ^ String.concat "" (List.map string_of_sstmt sfexpr.sfeBody) ^ "}"

and string_of_sfdecl sfdecl =
  "function " ^ string_of_typ sfdecl.sfdReturnType ^ " " ^
  sfdecl.sfdFname ^ "(" ^ String.concat ", " (List.map string_of_sbind sfdecl.sfdFormals) ^
  ")\n{\n" ^ String.concat "" (List.map string_of_sstmt sfdecl.sfdBody) ^ "}"


and string_of_sprogram sstmts = String.concat "" (List.map string_of_sstmt sstmts) ^ "\n"
