function void aFunction(int q, float s) {
    print(q);
    print(s);
    print("hello there!");
}

function void my_main() {
    int a = 1;
    string c = "c";
    float d = 1.0;
    int e = a;
    print(e);
    aFunction(e, 2.2);
}

my_main();