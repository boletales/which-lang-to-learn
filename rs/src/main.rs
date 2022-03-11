const MAX :usize = 100000000;
fn main() {
    println!("start");

    let mut sieve = vec![true; MAX+1];
    sieve[0] = false;
    sieve[1] = false;
    for i in 2..=(f32::sqrt(MAX as f32) as usize) {
        if sieve[i]{
            for j in (i*i..=MAX).step_by(i) {
                sieve[j] = false;
            }
        }
    }

    let mut primes = vec![0; MAX+1];
    let mut pcount = 0;
    for (i,s) in sieve.iter().enumerate() {
        if *s {
            primes[pcount] = i;
            pcount += 1;
        }
    }
    println!("{}",primes[pcount-1]);
    println!("end");
}
