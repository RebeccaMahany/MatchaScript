function void outer() {
    print("defining outer");
    function void inner() {
    	print("defining inner");
    }
 
    inner();
}

outer();

