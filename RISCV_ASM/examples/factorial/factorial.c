#include <stdio.h>
int factorial(int n) {
    if (n == 0) {
        return 1;
    }
    return n * factorial(n-1); 
}

int main() {
    int n;
    if (scanf("%d", &n) != 1) {
        return -1;	
    }
    printf("%d\n", factorial(n));
    return 0;
}
