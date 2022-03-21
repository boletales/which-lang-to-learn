import math
import numpy as np

print("start")
MAX = 100000000

sieve = np.ones(MAX+1, dtype="bool")

sieve[0] = False
sieve[1] = False

for i in range(2,int(math.sqrt(MAX))):
    if sieve[i]:
        sieve[np.arange(i,MAX//i + 1)*i] = False

nums   = np.arange(0,MAX+1)
primes = nums[sieve[nums]]
print("found "+ str(len(primes)) + " primes")
print(primes[-1])
print("end")
