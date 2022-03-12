main <- function(){
    print("start")
    
    max <- 100000000
    
    sieve <- rep(TRUE, max)
    
    sieve[1] = FALSE
    
    for(i in 1:floor(sqrt(max))){
        if(sieve[i]){
            #for(j in seq(i*i,max,by=i)) sieve[j] = FALSE
            sieve[(i:(max%/%i))*i] = FALSE
        }
    }
    
    nums   <- 1:max
    primes <- nums[sieve[nums]]
    
    print(primes[length(primes)])
    
    print("end")
}

main()
