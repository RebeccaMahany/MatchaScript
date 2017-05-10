function void primeNumberChecker(int a) {
	print("Current number:");
	print(a);
	int counter = 2;
	int prime = 1;
	int current_a = a;
	int b_mod = 0;
	if (a == 1)
	{
		print("This number is prime");
	}
	if (a<1)
	{
		print("A number greater than 0 please");
	}
	if (a>1)	
	{
		while (counter <= current_a)
		{
			b_mod = current_a % counter;
			if (b_mod ==0)	
			{
				if (counter != a)
				{
					prime = 0;
					print(counter);
					current_a = current_a / counter;
				}
				else
				{
					counter = counter+1;
				}
			}
			else
			{
				counter = counter+1;	
			}
		}
		
		if (prime==1)
		{	
			print("it's prime");
		}
	}
	
}
primeNumberChecker(5);
primeNumberChecker(27);
primeNumberChecker(43);