(* Scanner exceptions *)
exception IllegalCharacter of string * char * int

(* Parser exceptions *)
exception MissingEOF

(* Semantic analysis exceptions *)
exception IncorrectNumberOfArguments
exception IncorrectTypeOfArgument of string * string
exception ReturnTypeMismatch of string * string
exception UndefinedFunction of string
exception ConstructorNotFound
exception UndefinedClass of string
exception DuplicateConstructor
exception DuplicateField
exception DuplicateFunction of string
exception DuplicateLocal of string
exception DuplicateFormal of string
exception UndeclaredIdentifier of string
exception AssignmentTypeMismatch of string * string
exception VariableDeclarationTypeMismatch of string
exception InvalidIfStatementCondition
exception InvalidForStatementCondition
exception InvalidWhileStatementCondition
exception InvalidBinopEvalType (* "a" / "b" *)
exception InvalidBinaryOperator (* a @ b, where @ is not supported *)
exception InvalidUnaryOperator (* ^a *)
exception InvalidUnopEvalType (* !"not" *)
(*exception CallingBreakOutsideLoop
exception CallingContinueOutsideLoop*)

(* Codegen exceptions *)
exception InvalidTypePassedToPrint

(* MatchaScript exceptions *)
exception IncorrectNumberOfCompilerArguments of int
exception InvalidCompilerArgument
exception NoFileArgument
