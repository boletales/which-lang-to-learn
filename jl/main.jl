function main()
    MAX = 100000000
    
    println("start")
    
    sieve = fill(true,MAX)
    
    sieve[1] = false
    for i :: Int64 in 2:Int64(floor(sqrt(MAX)))
        if sieve[i]
            for j in (i*i):i:MAX
                sieve[j] = false
            end
        end
    end
    
    
    primes = fill(0,MAX)
    pcount = 1
    for i in 2:MAX
        if sieve[i]
            primes[pcount] = i
            pcount += 1
        end
    end
    
    println(primes[pcount-1])
    
    println("end")
end
main()
