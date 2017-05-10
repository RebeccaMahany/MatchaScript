function int whileLoop(int num) {
  int i = 0;
  if (num <= 0) {
    print("Next time run this program with a number higher than zero.");
    return -1;
  } else {
    while (i < num) {      
      print(i);
      i = i + 1;
    }
  }
  return 0;
}
whileLoop(5);
