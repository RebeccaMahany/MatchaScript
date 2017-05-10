class Rectangle {
  int height;
  int width; 

  constructor(int height, int width) {
    this.height = height;
    this.width = width;
  }
  
  int calcArea() {
    return this.height * this.width;
  }
}

Rectangle function getNewRect(int h, int w) {
  return new Rectangle(h, w);
};

int rectHeight = getNewRect(3, 5).height;

print(rectHeight); /* Should print 3 */

int rectHeight2 = new Rectangle(4,5).height;
print(rectHeight2); /* Should print 4 */
