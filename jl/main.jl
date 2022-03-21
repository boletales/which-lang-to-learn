function main()
    MAX = 100000000
    
    println("start")
    
    sieve = trues(MAX) :: BitVector
    
    sieve[1] = false
    @inbounds for i :: Int64 in 2:Int64(floor(sqrt(MAX)))
        if sieve[i]
            @inbounds for j in (i*i):i:MAX
                sieve[j] = false
            end
        end
    end

    primes = [i for i in 1:MAX if sieve[i]] :: Vector{Int64}
    
    println("found " * string(length(primes)) * " primes")
    println(primes[length(primes)])
    println("end")
end
main()
