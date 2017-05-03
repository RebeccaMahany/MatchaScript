/* anonymous function expression */
int a = function int() {
	return 3;
}

/* naming a function expression */
int a = function int bar() {
	return 3;
}

/*self invoking function expression */
(function void sayHello () {
	print("hello!");
})();

/*that last one may take  a while to work; that'll be fun */
