int outer(int firstNum) {
	int inner(int secondNum) {
		void evenInner() {
			void mostInner() {
				print("In mostInner()");
			}	
		}
		return firstNum + secondNum;
	}
	print inner(3);
	return firstNum;
}