/* based off https://www.typescriptlang.org/docs/handbook/classes.html */

class Greeter {
    string greeting;
    constructor(string message) {
        this.greeting = message;
    }
    string greet() {
        return "Hello, " + this.greeting;
    }
}

class Greeter greeter = new Greeter("world");
print(greeter.greet()); //returns Hello, world
