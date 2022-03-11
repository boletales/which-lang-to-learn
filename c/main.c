#include <stdio.h>
#include <stdbool.h>

const int MAX = 100000000;

int main(){
    static bool sieve[MAX+1];
    static int primes[MAX+1];
    puts("start");
    for(int i=0; i<=MAX; i++) sieve[i]=true;
    sieve[0] = false;
    sieve[1] = false;
    for(int i=0; i<=MAX; i++){
        if(sieve[i]){
            for(int j = i*i; j<=MAX; j+=i) sieve[j] = false;
        }
    }
    int pcount=0;
    for(int i=0; i<=MAX; i++){
        if(sieve[i]){
            primes[pcount]=i;
            pcount++;
        }
    }

    printf("%d\n", primes[pcount-1]);
    puts("end");
    return 0;
}
