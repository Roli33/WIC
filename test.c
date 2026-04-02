#include <stdio.h>

int x = 0;

int* alma(int* p)
{
    x++;
    *p = 0;
    return p;
}

int main()
{
    x = 1;
    f(x);
    x = 100;
    f (x);
    int p = 5;
    *alma(&p) = 10;
    printf("%d\n", x);
    return 0;
}