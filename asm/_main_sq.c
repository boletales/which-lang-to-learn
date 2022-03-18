#include <stdio.h>
#include <stdbool.h>

#define uchar unsigned char
#define uint  unsigned int
#define ull   unsigned long long

const uint MAX = 100000000;
uchar sieve[MAX/8+1];
uint  primes[MAX+1];


static uint isqrt(uint s) {
	uint x0, x1 = s / 2;
    if (x1 == 0) return s;
    do {
        x0 = x1;
	    x1 = (x0 + s / x0)/2;
    } while (x1 < x0);
	return x0;
}


int main(){
    puts("start");
    sieve[0] = 0b11;

    uint sqrt_max = isqrt(MAX);
    for(uint i=0; i<= sqrt_max; i++){
        if(sieve[i>>3] & (1<<(i&7))) {
            continue;
        }
        for(uint j = i * i; j<=MAX; j+=i) {
            uint jj = j >> 3;
            uchar v = sieve[jj];
            v |= (1 << (j&7));
            sieve[jj] = v;
        }
    }
    uint pcount = -1;
    for(uint i=0; i < MAX / 8; i++){
        uchar v = sieve[i];
        uint ii = i << 3;
        if ((v & 0b00000001) == 0) primes[++pcount] = ii + 0;
        if ((v & 0b00000010) == 0) primes[++pcount] = ii + 1;
        if ((v & 0b00000100) == 0) primes[++pcount] = ii + 2;
        if ((v & 0b00001000) == 0) primes[++pcount] = ii + 3;
        if ((v & 0b00010000) == 0) primes[++pcount] = ii + 4;
        if ((v & 0b00100000) == 0) primes[++pcount] = ii + 5;
        if ((v & 0b01000000) == 0) primes[++pcount] = ii + 6;
        if ((v & 0b10000000) == 0) primes[++pcount] = ii + 7;
    }
    printf("%d\nend\n", primes[pcount]); // 99999989
    printf("count-1 %d\n\n", pcount);    // 5761454
}