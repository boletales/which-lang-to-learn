import math
from numba import jit
from numba import prange

print("start")

@jit(cache=True, parallel=True)
def main():
    MAX = 100000000
    sieve = [True]*(MAX+1)

    sieve[0] = False
    sieve[1] = False

    for l in range(1,int(math.log(math.sqrt(MAX))/math.log(2)+1)):
        for i in prange(1<<l,min(2<<l, int(math.sqrt(MAX)))):
            if sieve[i]:
                for j in range(i*i,MAX+1,i):
                    sieve[j] = False

    primes = [0]*(MAX+1)
    pcount = 0

    for i in range(2,MAX+1):
        if sieve[i]:
            primes[pcount] = i
            pcount += 1

    print("found "+ str(pcount) + " primes")
    print(primes[pcount-1])

main()
print("end")
