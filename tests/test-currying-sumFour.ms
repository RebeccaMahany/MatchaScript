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

/* Original Javascript:
function sumFour(w) {
  return function (x) {
    return function (y) {
      return function (z) {
        return w + x + y + z;
      }
    }
  }
}

sumFour(1)(2)(3)(4); // 10
*/