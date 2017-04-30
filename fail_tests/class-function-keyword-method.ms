class Person {
  string name;  /* all fields are public by default */

  constructor(string nm) {
    this.name = nm;
  }
	
  /* there should not be a function keyword here*/
  function void changeName(string newName) {
    this.name = newName;
  }
}

class Person p1 = new Person("ruijia");
p1.changeName("rachel");
print(p1.name);   /* Outputs "rachel" */