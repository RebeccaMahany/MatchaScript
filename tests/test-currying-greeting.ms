fun greetCurried = function fun (string greeting) {
  return function void (string name) {
    print(greeting);
    print(", ");
    print(name);
  };
};


fun greetHello = greetCurried("Hello");
greetHello("Heidi"); // "Hello, Heidi"

greetCurried("Hi there")("Howard"); // "Hi there, Howard"
/* How to handle that in codegen? */