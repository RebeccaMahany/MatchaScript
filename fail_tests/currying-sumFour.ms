function fun sumFour(int w) {
  return function fun (int x) {
    return function fun (int y) {
      return function int (int z) {
        return w + x + y + z;
      };
    };
  };
};	/* there should not be a semicolon at the end of a function declaration */

int x = sumFour(1)(2)(3)(4); /* fully applied; x equals 10 */