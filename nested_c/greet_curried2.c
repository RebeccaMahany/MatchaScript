#include<stdlib.h>
#include<stdio.h>

/********** all variable accesses are replaced by struct accesses *********/

/********** Forward Declarations **********/
struct Rec_toplevel;
struct Rec_toplevel_anon0;
struct Rec_toplevel_anon0_anon0;
struct Fp_toplevel_anon0_anon0;

/***************** Functions *****************/
// Function forward declarations are made 
struct Rec_toplevel_anon0_anon0 *toplevel_anon0(struct Rec_toplevel *parent, char *greeting);
void toplevel_anon0_anon0(struct Rec_greetCurried *parent, char *name);

/***************** Structs *****************/
struct Rec_toplevel {
	//functions
	struct Fp_toplevel_anon0_anon0 (*(*greetCurried)(struct Rec_toplevel *parent, char *greeting));

	// locals
	// all function pointers are returned as a struct containing the function's static 
	// scoping parent and parameters
	struct Fp_toplevel_anon0_anon0 *greetHello;
};

// Declare access link structs for each function
struct greetCurried_anon0 {
	// pointer to parent struct
	struct Rec_toplevel *parent;
	// all formals and locally declared variables, functions, and classes
	// formals
	char *greeting;
	// locals
	// functions
	void (*greetCurried_anon0_anon0)(struct Rec_greetCurried *, char *);
	// classes
};

struct Rec_greetCurried_anon0_anon0 {
	// pointer to parent struct
	struct Rec_greetCurried *parent;
	// all formals and locally declared variables, functions, and classes
	// formals
	char *name;
	// locals
	// classes
};

// make structs for all returned functions. each struct contains:
// 1. a function pointer to the returned function
// 2. the environment of the parent of the returned function (static scope)
struct Fp_toplevel_anon0_anon0 {
	void (*fp)(struct Rec_greetCurried *parent, char *name);
	struct Rec_greetCurried *env;
};

struct Fp_toplevel_anon0_anon0 *toplevel_anon0(struct Rec_toplevel *parent, char *greeting) {
	struct Rec_greetCurried *mine = malloc(sizeof(struct Rec_greetCurried));
	mine->parent = parent;
	mine->greeting = greeting; 	// formals, assigns, and vdecls

	// return a struct containing:
	// 1. function pointer to 
	// 2. 
	struct Fp_toplevel_anon0_anon0 *fp = malloc(sizeof(struct Fp_toplevel_anon0_anon0));
	fp->fp = &greetCurried_anon0_anon0;
	fp->env = mine;
	return fp;	// 
}

void greetCurried_anon0_anon0(struct Rec_greetCurried *parent, char *name) {
	struct Rec_greetCurried_anon0_anon0 *mine = malloc(sizeof(struct Rec_greetCurried_anon0_anon0));
	// fill in formals at the beginning of every scope
	mine->parent = parent;
	mine->name = name;

	// for each variable access, determine (using symbol tables in codegen) whether it 
	// belongs to yourself, to the parent, or to one of the parents' parents
	// the parent of a returned function expression is the next outermost scope for the 
	// returned function expression
	printf("%s, ", parent->greeting);
	printf("%s\n", mine->name);
}

int main(int argc, char **argv) {
	struct Rec_toplevel *mine = malloc(sizeof(struct Rec_toplevel));
	mine->greetCurried = &toplevel_anon0;
	// when you call a function, locate where that function is relative to you
	// is it in your symbol table? --> pass in "mine"
	// is it in the symbol table above? --> pass in "parent"
	mine->greetHello = (mine->greetCurried)(mine, "Hello");
	// when you are calling a returned function, the returned function pointer also returns an env
	(mine->greetHello->fp)(mine->greetHello->env, "Heidi");
	
	(mine->greetCurried)(mine, "Hi there")
}