import cylib

print("start")
(primes,pcount) = cylib.main()
print("found "+ str(pcount) + " primes")
print(primes[pcount-1])
print("end")
