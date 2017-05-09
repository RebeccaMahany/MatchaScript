let funTwoConstructs = {
	stmts = [Expr(IntLit(2))]
}
let funTwo = {
  fdReturnType = Void;
  fdFname = "goodbye";
  fdFormals= [];
  fdBody = funTwoConstructs
}

let funOneConstructs = {
	stmts= [FDeclStmt(funTwo)]
} 
let funOne = { 
  fdReturnType = Void;
  fdFname = "hello";
  fdFormals= [];
  fdBody = funOneConstructs
} 
let constructs = {
	stmts= [FDeclStmt(funOne)]
}



let sast = toplevel_fdecl