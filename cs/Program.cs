// See https://aka.ms/new-console-template for more information
Console.WriteLine("start");
const int max = 100000000;
bool[] sieve = Enumerable.Repeat(true, max + 1).ToArray();
var sqrtmax = (int)Math.Sqrt(max);
for(var i = 2; i <= sqrtmax; i++){
    if(sieve[i]){
      for(var j = i*i; j <= max; j+=i){
            sieve[j] = false;
      }
    }
}
var primes = new int[max + 1];
var pcount = 0;
for(var i = 2; i <= max; i++){
    if(sieve[i]){
        primes[pcount] = i;
        pcount += 1;
    }
}

Console.WriteLine(primes[pcount - 1]);
Console.WriteLine("end");