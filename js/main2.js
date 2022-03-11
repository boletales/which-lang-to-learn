console.log("start");

const max = 100000000;

let sieve = Array(max+1).fill(1);
sieve[0] = 0;
sieve[1] = 0;

for(let i=2; i<=(Math.floor(Math.sqrt(max)) | 0); i++){
    if(sieve[i]){
        for(let j=i*i; j<=max; j+=i) sieve[j] = false;
    }
}

let primes = Array(max+1).fill(0);
let pcount = 0;

for(let i=2; i<=max; i++){
    if(sieve[i]){
        primes[pcount] = i;
        pcount++;
    }
}

console.log(primes[pcount-1]);

console.log("end")
