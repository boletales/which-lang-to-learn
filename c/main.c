#include <stdio.h>
#include <stdbool.h>
#include <math.h>
#include <string.h>

const int MAX = 100000000;

int main() {
    int SQRT_MAX = (int)sqrt((double)MAX);
    static bool sieve[MAX + 1];
    static int primes[MAX + 1];
    puts("start");
    memset(sieve, 1, (MAX + 1) * sizeof(sieve[0]));
    sieve[0] = false;
    sieve[1] = false;
    for (int i = 0; i <= SQRT_MAX; i++) {
        if (sieve[i]) {
            for (int j = i * i; j <= MAX; j += i) sieve[j] = false;
        }
    }
    int pcount = 0;
    for (int i = 0; i <= MAX; i++) {
        if (sieve[i]) {
            primes[pcount] = i;
            pcount++;
        }
    }

    printf("%d\n", primes[pcount - 1]);
    puts("end");
    return 0;
}
