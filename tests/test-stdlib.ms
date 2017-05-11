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
    if (i > 0.0) {
        return i;
    }
    return -1.0 * i;
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
/*
function float float_max(float a, float b){
    if (a > b){
        return a;
    } else {
        return b;
    }
}
*/

/** Get min of two ints **/
function int int_min(int a, int b){
    if (a < b){
        return a;
    } else {
        return b;
    }
}


/** Get min of two floats **/
/*
function float float_min(float a, float b){
    if (a < b){
        return a;
    } else {
        return b;
    }
}
*/

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


/** Get ceiling of float **/
function float ceil(float f) {
    float remainder = f % 1.0;
    float new_f = f + 1.0 - remainder;
    return new_f;
}


/** Get floor of float, returns int **/
function float floor(float f) {
    float remainder = f % 1.0;
    float new_f = f - remainder;
    return new_f;
}


/** Round a float to the decimal place specified **/
function float round(float f) {
    float absOfF = float_abs(f);
    float floorOfF = floor(f);
    float result;

    /* Determine whether we are closer to the ceiling of f */
    if (absOfF - floorOfF >= 0.5) {
        result = floorOfF + 1.0;
    /* Else we are closer to the floor of f */
    } else {                
        result = floorOfF;
    }
    return result;
}
