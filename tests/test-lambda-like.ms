/* lambda version: (&x.&y. x) a b
returns a
 */

/* what is the type of a? */
/*
fun a = function int(int x) {
	return function int(int y) {
		return x;
	};
};
*/
/*fun b = a(3);*/ /* b is a function that takes in a number and returns 3 regardless */

/*print(b(5));*//* should output 3 */

/***************************
all of the above is commented out for now
****************************/

print((function fun(int x) { 
	return function int (int y) {
		return x;
	};
})(99)(4)); /* should output 99*/

/* what if there was no argument 4? How would semant check that print should not accept a function expression as input? */
