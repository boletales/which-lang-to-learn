puts "start"

MAX = 100000000
sieve = Array.new(MAX + 1, true)
sieve[0] = sieve[1] = false

2.upto(Integer.sqrt(MAX)) do |i|
  (i * i).step(by: i, to: MAX) { |j| sieve[j] = false } if sieve[i]
end

primes = sieve.filter_map.with_index { |v, i| i if v }
puts primes.last, "end"
