void main() {

    int i = 0;

    while (i < 5){
        print(i);
        if (i == 2) { break ; }
        i = i + 1;
    }

    int j = 0;

    while (j < 5) {
        print(j);
        if (j == 2) { continue ; }
        j = j + 1;
    }

}
