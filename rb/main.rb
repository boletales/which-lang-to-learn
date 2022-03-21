require 'numo/narray'

puts "start"

MAX = 100000000
sieve = Numo::Bit.ones(MAX + 1)
sieve[0] = sieve[1] = 0

2.upto(Integer.sqrt(MAX)) do |i|
  sieve[((i * i)..-1) % i] = 0 if sieve[i]
end

primes = Numo::Int32.new(MAX + 1).seq(0, 1)[sieve[0..MAX]]
puts "found #{primes.size} primes"
puts primes[-1], "end"
