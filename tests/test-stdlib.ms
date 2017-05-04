/***************
Error objects
***************/
class Error{

    /** Class fields **/
    string message; /** Error message **/
    string name; /** Type of error **/

    /** Constructor with error message **/
    constructor(string msg) {
        this.name = "Error";
        this.message = msg;
    }
}



/*************** 
Functions for converting types 
***************/


/** String to bool **/
function bool stringToBool(string s) {
    if (s == "true"){
        return true;
    }
    if (s == "True"){
        return true;
    }
    if (s == "false"){
        return false;
    }
    if (s == "False"){
        return false;
    }
}


/** String to int **/


/** String to float **/


/** Bool to string **/
function string boolToString(bool b) {
    if (b){
        return "true";
    } else {
        return "false";
    }
}


/** Int to string **/


/** Int to float **/


/** Float to string **/


/** Float to int **/



/*************** 
Math functions 
***************/


/** Absolute value for integers **/
function int int_abs(int i) {
    if (i > 0) {
        return i;
    } else {
        return -1 * i;
    }
}


/** Absolute value for floats **/
function float float_abs(float i) {
    if (i > 0) {
        return i;
    }
    return -1.0 * i;
}


/** Random function to return random float between 0 and 1 **/
function float rand() {
    return 0.0;
}


/** Get max of two ints **/
function int int_max(int a, int b){
    if (a > b){
        return a;
    } else {
        return b;
    }   
}


/** Get max of two floats **/
function float float_max(float a, float b){
    if (a > b){
        return a;
    } else {
        return b;
    }
}


/** Get min of two ints **/
function int int_min(int a, int b){
    if (a < b){
        return a;
    } else {
        return b;
    }
}


/** Get min of two floats **/
function float float_min(float a, float b){
    if (a < b){
        return a;
    } else {
        return b;
    }
}


/** Raise an integer to a power **/
function int int_pow(int base, int pow){
    int i = 1;
    int result = base;
    while (i < pow){
        result = result * base;
        i = i + 1;
    }
    return result;
}


/** Raise a float to a power **/
function float float_pow(float base, int pow){
    int i = 1;
    float result = base;
    while (i < pow){
        result = result * base;
        i = i + 1;
    }
    return result;
}


/** Get ceiling of float, returns int **/
function int ceil(float f) {
    remainder = f % 1;
    new_f = f + 1 - remainder;
    return floatToInt(new_f);
}


/** Get floor of float, returns int **/
function int floor(float f) {
    remainder = f % 1;
    new_f = f - remainder;
    return floatToInt(new_f);
}


/** Round a float to the decimal place specified **/
