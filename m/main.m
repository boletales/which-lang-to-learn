function main()
    MAX=100000000;
    disp("start");
    sieve = true(1,MAX);
    sieve(1) = 0;
    for i = 2:int32(sqrt(MAX))
        if sieve(i)
            for j = (i*i):i:MAX
                sieve(j) = false;
            end
        end
    end
    primes=zeros(1,sum(sieve),'int32');
    count=1;
    for i =1:MAX
        if sieve(i)
            primes(count)=i;
            count=count+1;
        end
    end
    disp("found " + string(length(primes)) + " primes")
    disp(primes(end))
    disp("end")
end