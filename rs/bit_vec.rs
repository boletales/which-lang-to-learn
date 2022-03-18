const MAX: usize = 100000000;
fn main() {
    println!("start");

    let mut sieve = vec![0u64; MAX / 64 + 1];
    sieve[0] ^= 3;
    let sqrtmax = f32::sqrt(MAX as f32) as usize;
    for i in 2..=sqrtmax {
        if (sieve[i / 64] >> i % 64) & 1 == 0 {
            let mut j = i * i;
            while j <= MAX {
                sieve[j / 64] |= 1 << j % 64;
                j += i;
            }
        }
    }

    let mut primes = Vec::new();
    for i in 2..=MAX {
        if (sieve[i / 64] >> i % 64) & 1 == 0 {
            primes.push(i);
        }
    }
    println!("{}", primes.last().unwrap());
    println!("end");
}
