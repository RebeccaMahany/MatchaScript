#include "include/stdlib.ms";


int a = int_abs(-5);
print("absolute value of int -5 is: ");
print(a);
print();

float b = float_abs(-3.233);
print("absolute value of float -3.233 is: ");
print(a);
print();

int c = int_max(2, 300);
print("max value between ints 2 and 300 is: ");
print(c);
print();

int e = int_min(1, 3);
print("min value between ints 1 and 3 is: ");
print(e);
print();

int g = int_pow(2, 5);
print("2 to the 5 is: ");
print(g);
print();

int h = float_pow(2.0, 5);
print("2.0 to the 5 is: ");
print(h);
print();

float i = ceil(1.333333);
print("ceiling of 1.333333 is: ");
print(i);
print();

float j = floor(1.333333);
print("floor of 1.333333 is: ");
print(j);
print();

float k = round(2.444449);
print("rounding 2.444449 to the nearest whole number: ");
print(k);
print();