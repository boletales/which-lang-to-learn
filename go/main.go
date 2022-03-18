package main

import (
	"fmt"
	"math"
)

func main() {
	fmt.Printf("start\n")
	const max = 100000000
	var sqrtmax = int(math.Sqrt(max))
	sieve := [max + 1]bool{}
	for i := 0; i <= max; i++ {
		sieve[i] = true
	}
	sieve[0] = false
	sieve[1] = false
	for i := 0; i <= sqrtmax; i++ {
		if sieve[i] {
			for j := i * i; j <= max; j += i {
				sieve[j] = false
			}
		}
	}
	primes := [max + 1]int{}
	var pcount = 0
	for i := 0; i <= max; i++ {
		if sieve[i] {
			primes[pcount] = i
			pcount++
		}
	}
	fmt.Printf("%d\n", primes[pcount-1])
	fmt.Printf("end\n")
}
