#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

// --- User Directives ---
// -----------------------

int main(int n, double x, double y, bool option) {
    double dif = (x - y);
    if (option) {
    {
        int res = (dif / n);
        assert((dif > (-9999)));
        return res;
    }
    } else {
    {
        int res = dif;
        assert((dif > (-9999)));
        return res;
    }
    }

}

void example(int x) {
    x = (x + 5);
    return x;

}