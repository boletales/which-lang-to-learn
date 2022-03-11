main <- function(){
    print("start")
    
    max <- 100000000
    
    sieve <- rep(TRUE, max)
    
    sieve[1] = FALSE
    
    for(i in 1:floor(sqrt(max))){
        if(sieve[i]){
            for(j in seq(i*i,max,by=i)) sieve[j] = FALSE
        }
    }
    
    primes <- rep(0, max)
    pcount <- 0;
    
    for(i in 1:max){
        if(sieve[i]){
            primes[pcount] = i
            pcount = pcount + 1
        }
    }
    
    print(primes[pcount-1])
    
    print("end")
}

main()
