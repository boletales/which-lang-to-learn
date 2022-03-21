printfn "start"

let max = 100000000
let sqrtmax = int (sqrt (double max))
let mutable sieve = Array.create (max+1) true

sieve[0] <- false
sieve[1] <- false
for i in 2 .. sqrtmax do
  if sieve[i] then
    for j in i*i .. i .. max do
      sieve[j] <- false
    ()

let mutable primes = Array.create (max+1) 0
let mutable pcount = 0
for i in 2 .. max do
  if sieve[i] then
    primes[pcount] <- i
    pcount <- pcount+1
    ()

printfn "found %d primes" pcount
printfn "%d" primes[pcount-1]

printfn "end"