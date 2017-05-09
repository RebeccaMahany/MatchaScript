#include<stdlib.h>
#include<stdio.h>

/*
Implement what greetCurried would look like in C

fun greetCurried = function fun (string greeting) {
  return function void (string name) {
    print(greeting);
    print(", ");
    print(name);
  };
};


fun greetHello = greetCurried("Hello");
greetHello("Heidi"); // "Hello, Heidi"

greetCurried("Hi there")("Howard"); // "Hi there, Howard"
===========================================================
function string greetCurried(string greeting) {
	function void greetCurried_anon1(string name) {
		print(greeting);
		print(name);	
	}
	return greetCurried_anon1;
}
*/

/***************** Structs *****************/
struct Rec_toplevel {
	// functions
};

// Declare access link structs for each function
struct Rec_greetCurried {
	// pointer to parent struct
	struct Rec_toplevel *parent;
	// all formals and locally declared variables, functions, and classes
	// formals
	char *greeting;
	// locals
	// functions
	void (*greetCurried_anon1)(char *);
	// classes
};

struct Rec_greetCurried_anon1 {
	// pointer to parent struct
	struct Rec_greetCurried *parent;
	// all formals and locally declared variables, functions, and classes
	// formals
	char *name;
	// locals
	// functions
	// classes
};

struct Fp_greetCurried_anon1 {
	void (*fp)(struct Rec_greetCurried *, char *);
	struct Rec_greetCurried *env;
};

/***************** Functions *****************/
void greetCurried_anon1(struct Rec_greetCurried *parent, char *name);

struct Fp_greetCurried_anon1 *greetCurried(struct Rec_toplevel *parent, char *greeting) {
	struct Rec_greetCurried *mine = malloc(sizeof(struct Rec_greetCurried));
	mine->parent = parent;
	mine->greeting = greeting;
	// return a struct containing:
	// 1. function pointer to greetCurried_anon1 and
	// 2. "mine" struct (context within greetCurried)
	struct Fp_greetCurried_anon1 *fp = malloc(sizeof(struct Fp_greetCurried_anon1));
	fp->fp = &greetCurried_anon1;
	fp->env = mine;
	return fp;
}

void greetCurried_anon1(struct Rec_greetCurried *parent, char *name) {
	printf("%s, ", parent->greeting);
	printf("%s\n", name);
}

int main(int argc, char **argv) {
	struct Rec_toplevel *mine = malloc(sizeof(struct Rec_toplevel));

	struct Fp_greetCurried_anon1 *greetHello = greetCurried(mine, "Hello");
	(greetHello->fp)(greetHello->env, "Heidi");

	struct Fp_greetCurried_anon1 *greetCurried_anon1 greetCurried(mine, "Hi there");
}