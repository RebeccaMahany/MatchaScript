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
exception DuplicateGlobal of string
exception UndeclaredIdentifier of string
exception AssignmentTypeMismatch of string * string
exception VariableDeclarationTypeMismatch of string
(*exception CallingBreakOutsideLoop
exception CallingContinueOutsideLoop*)

(* Codegen exceptions *)
exception InvalidTypePassedToPrint
exception InvalidBinaryOperator (* a @ b, where @ is not supported *)
exception InvalidBinopEvalType (* "a" / "b" *)
exception InvalidUnaryOperator (* ^a *)
exception InvalidUnopEvalType (* !"not" *)
exception UndefinedID of string

(* MatchaScript exceptions *)
exception IncorrectNumberOfCompilerArguments of int
exception InvalidCompilerArgument of string
exception NoFileArgument
