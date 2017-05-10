function void outer() {
	function void inner() { 
		int a = 5;
	}
	print(a);	/* a not in scope */ 
}

outer();