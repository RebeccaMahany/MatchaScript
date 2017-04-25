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
exception UnknownID of string
exception AssignmentTypeMismatch of string * string
exception CallingBreakOutsideLoop
exception CallingContinueOutsideLoop

(* Codegen exceptions *)
exception InvalidTypePassedToPrintf
exception InvalidBinaryOperator
exception InvalidBinopEvalType
exception InvalidUnopType
exception InvalidUnopEvalType
exception UnknownVariable of string

(* MatchaScript exceptions *)
exception IncorrectNumberOfCompilerArguments of int
exception InvalidCompilerArgument of string
exception NoFileArgument
