#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Runtime functions for BASIC compiler

void print_double(double x) {
    printf("%g\n", x);
}

void print_string(const char* s) {
    printf("%s\n", s);
}

double input_double() {
    double value;
    if (scanf("%lf", &value) == 1) {
        // Clear input buffer
        int c;
        while ((c = getchar()) != '\n' && c != EOF);
        return value;
    }
    return 0.0;
}

double read_string_as_double(const char* s) {
    return atof(s);
}
