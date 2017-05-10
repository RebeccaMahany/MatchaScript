function int primeNumberChecker(int a) {
	print(a);
	int counter = 2;
	int current = 1;
	int b_mod = 0;
	if (a == 1)
	{
		print("this is prime");
	}
	if (a<1)
	{
		print("A number greater than 0 please");
	}
	if (a>1)	
	{
		while (counter < a)
		{
			b_mod = a % counter;
			if (b_mod ==0)	
			{
				current = 0;
			}
			counter = counter + 1;
		}
		
		if (current==1)
		{	
			print("it's prime");
		}
		else	
		{
			print("it's not prime");
		}	
		//print("this worked");
	}
}
primeNumberChecker(5);
primeNumberChecker(25);
