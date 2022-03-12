import java.util.Arrays;
class Primes {
    public static void main(String[] args){
        System.out.println("start");
        final int max = 100000000;
        boolean[] sieve = new boolean[max+1];
        Arrays.fill(sieve, true);
        var sqrtmax = (int)Math.sqrt(max);
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
        System.out.println(primes[pcount-1]);
        
        System.out.println("end");
    }
}
