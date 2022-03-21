console.log("start");

const max = 100000000;

let sieve = new Int8Array(max+1);
for(let i=2; i<=max; i++) sieve[i] = 1;

for(let i=2; i<=(Math.floor(Math.sqrt(max)) | 0); i++){
    if(sieve[i]){
        for(let j=i*i; j<=max; j+=i) sieve[j] = false;
    }
}

let primes = new Int32Array(max+1);
let pcount = 0;

for(let i=2; i<=max; i++){
    if(sieve[i]){
        primes[pcount] = i;
        pcount++;
    }
}

console.log("found " + pcount + " primes");
console.log(primes[pcount-1]);

console.log("end")
