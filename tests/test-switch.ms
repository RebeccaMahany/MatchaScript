function void main() {

    string hello;
    switch (0) {
        case 1:
            hello = "No hello for you";
        case 2:
            hello = "No hello for you either";
        default:
            hello = "Hello there!";
    }
    print(hello);

    function int returnsOne() {
        return 1;
    }

    int i;
    switch ( returnsOne() ) {
        case 1:
            i = 1;

        case 2:
            i = 2;

        default:
            i = 0;
    }
    print(i);

    string day = "Sunday";
    string today;
    switch ( day ) {
        default:
            today = "I don't know what day today is!";
        case "Sunday":
            today = "Can't believe the weekend is almost over :(";
    }
    print(today);

}
