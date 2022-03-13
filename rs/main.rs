const MAX :usize = 100000000;
fn main() {
    println!("start");

    let mut sieve = vec![true; MAX+1];
    sieve[0] = false;
    sieve[1] = false;
    let sqrtmax = f32::sqrt(MAX as f32) as usize;
    for i in 2..=sqrtmax {
        if sieve[i]{
            for j in i..=MAX/i {
                sieve[j*i] = false;
            }
        }
    }

    let mut primes = vec![0; MAX+1];
    let mut pcount = 0;
    for i in 2..=MAX {
        if sieve[i] {
            primes[pcount] = i;
            pcount += 1;
        }
    }
    println!("{}",primes[pcount-1]);
    println!("end");
}
