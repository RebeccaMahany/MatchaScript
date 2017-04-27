function fun sumFour(int w) {
  return function fun (int x) {
    return function fun (int y) {
      return function int (int z) {
        return w + x + y + z;
      };
    };
  };
}

int x = sumFour(1)(2)(3)(4); /* fully applied; x equals 10 */