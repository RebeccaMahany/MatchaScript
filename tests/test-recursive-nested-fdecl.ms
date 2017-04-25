function void outer() {
    print("defining outer");
    function void inner() {
    	print("defining inner");
    }
    outer();
}

/*
Pretty prints as:

function void outer()
{
print("defining outer");
outer();					// function declarations always printed after stmts in a scope!
							// because constructs is a { stmt list; fdecl list; },
							// they are pretty-printed in that order.
							// this is a problem!
function void inner()
{
print("defining inner");
}
}

*/