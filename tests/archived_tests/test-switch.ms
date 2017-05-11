function void main() {

    string hello;
    switch (0) {
        case 1:
            hello = "No hello for you";
            break;
        case 2:
            hello = "No hello for you either";
            break;
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
            break;

        case 2:
            i = 2;
            break;

        default:
            i = 0;
    }
    print(i);

    string day = "Sunday";
    string today;
    switch ( day ) {
        default:
            today = "I don't know what day today is!";
            break;
        case "Sunday":
            today = "Can't believe the weekend is almost over :(";
    }
    print(today);

}
