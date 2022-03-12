function main()
    MAX = 100000000
    
    println("start")
    
    sieve = trues(MAX)
    
    sieve[1] = false
    @inbounds for i :: Int64 in 2:Int64(floor(sqrt(MAX)))
        if sieve[i]
            @inbounds for j in (i*i):i:MAX
                sieve[j] = false
            end
        end
    end
    
    
    primes = zeros(Int64,MAX)
    pcount = 1
    @inbounds for i in 2:MAX
        if sieve[i]
            primes[pcount] = i
            pcount += 1
        end
    end
    
    println(primes[pcount-1])
    
    println("end")
end
main()
