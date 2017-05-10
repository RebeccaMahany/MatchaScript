class Person
{
  string name;  // all fields are public by default

  void changeName(string newName) {
    this.name = newName;
  }
}

Person p1 = new Person("ruijia");
p1.changeName("rachel");
print(p1.name);   // Outputs "rachel"
