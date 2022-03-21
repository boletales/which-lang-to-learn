print("start");

local max = 100000000
local sqrtmax = math.sqrt(max)
local sieve = {}
for i=0, max do
  sieve[i] = true
end
sieve[0] = 0
sieve[1] = 0

for i=2, sqrtmax do
  if sieve[i] then
    for j = i*i, max, i do
      sieve[j] = false
    end
  end
end
local primes = {}
local pcount = 0
for i=2, max do
  if sieve[i] then
    primes[pcount] = i
    pcount = pcount + 1
  end
end
print("found " .. tostring(pcount) .. " primes");
print(primes[pcount-1]);

print("end");
