function int fib_rec (int n) {
	if (n == 0) {
		return 0;
	}
	if (n == 1) {
		return 1;
	}
	return fib_rec(n-1) + fib_rec(n-2);
}

//int result = fib_rec(6);
//print(result);