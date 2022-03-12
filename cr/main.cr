puts "start"

MAX = 100000000
sieve = Array.new(MAX + 1, true)
sieve[0] = sieve[1] = false

2.upto(Math.isqrt(MAX)) do |i|
  (i * i).step(by: i, to: MAX) { |j| sieve[j] = false } if sieve[i]
end

primes = (1..MAX).select{ |i| sieve[i] }
puts primes.last, "end"
