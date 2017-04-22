%{
open Ast
%}

%token FUNCTION CLASS CONSTRUCTOR THIS NEW
%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA LBRACKET RBRACKET DOT
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE DO INT FLOAT BOOL CHAR STRING VOID FUN
%token <int> INTLIT
%token <float> FLOATLIT
%token <char> CHARLIT
%token <string> STRINGLIT
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT NEG DOT

%start program
%type <Ast.program> program

%%

program:
  stmt_list EOF { Program($1) }

/*********
typs
**********/
typ:
    INT    { Int }
  | FLOAT  { Float }
  | BOOL   { Bool }
  | CHAR   { Char }
  | STRING { String }
  | VOID   { Void }
  | FUN    { Fun }
  | CLASS ID { ObjectType($2) }

/*********
Variables
**********/
vdecl:
    typ ID SEMI { ($1, $2, Noexpr)}
  | typ ID ASSIGN expr SEMI { ($1, $2, $4) }

/*********
Functions
**********/
fdecl:
   FUNCTION typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
     { { fdReturnType = $2;
   fdFname = $3;
   fdFormals = $5;
   fdBody = $8 } }

fexpr:
  FUNCTION typ LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
  { { feReturnType = $2;
   feFormals = $4;
   feBody = $7 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }


/*********
Statements
**********/
stmt_list:
    stmt  { [$1] }
  | stmt_list stmt { $1@[$2] } /* append statement to end of statement list */

stmt:
    expr SEMI { ExprStmt $1 }
  | vdecl { VarDecl($1) }
  | fdecl { FunDecl($1) }
  | RETURN expr SEMI { Return $2 }
  | RETURN SEMI { Return Noexpr }
  | LBRACE stmt_list RBRACE { Block($2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | DO stmt WHILE LPAREN expr RPAREN SEMI { DoWhile ($2, $5) }

/*********
Expressions
**********/
expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

primary_expr:
    THIS          { This }
  | ID            { Id($1) }

call_expr:
    ID            { Id($1) }
  | call_expr LPAREN actuals_opt RPAREN  { CallExpr($1, $3) }

expr:
    INTLIT           { IntLit($1)           }
  | FLOATLIT         { FloatLit($1)         }
  | CHARLIT          { CharLit($1)          }
  | STRINGLIT        { StringLit($1)        }
  | TRUE             { BoolLit(true)        }
  | FALSE            { BoolLit(false)       }
  | ID               { Id($1)               }
  | fexpr            { FunExpr($1)          }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr MOD    expr { Binop($1, Mod,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | NOT expr         { Unop(Not, $2) }
  | expr ASSIGN expr   { Assign($1, $3) }
  | call_expr { $1 }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
