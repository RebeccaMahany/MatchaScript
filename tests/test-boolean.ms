function void testBoolean(bool x, bool y) {
  if (x == y) {
    print("X is equal to Y");
  } else {
    print("X is not equal to Y");
  }

  bool z = true;
  if (x && z) {
    print("X and Z are both true");
  } else if (x || z) {
    print("X or Z is true");
  } else {
    print("X and Z are both false");
  }
}

testBoolean(true, false);
