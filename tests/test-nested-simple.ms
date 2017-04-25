function int outer(int firstNum) {
	function int inner(int secondNum) {
		function void evenInner() {
			function void mostInner() {
				print("In mostInner()");
			}	
		}
		return firstNum + secondNum;
	}
	print(inner(3));
	return firstNum;
}
