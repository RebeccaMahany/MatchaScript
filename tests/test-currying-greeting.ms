fun greetCurried = function fun (string greeting) {
  return function void (string name) {
    print(greeting);
    print(", ");
    print(name);
    return function void (string punctuation) {
    	print(punctuation);
    }
  };
};


fun greetHello = greetCurried("Hello");
greetHello("Heidi"); // "Hello, Heidi"

greetCurried("Hi there")("Howard"); // "Hi there, Howard"