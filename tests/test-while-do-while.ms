function void main() {

    int i = 0;

    do {
        print(i);
        i = i + 1;
    }
    while ( i < 4 );

    int j = 1;
    do {
        print("I should be printed once!");
    }
    while (j < 1);

    int k = 1;
    while (i < 5) {
        print(i);
        i = i + 1;
    }

}
