<?php
print "start\n";

ini_set('memory_limit', '6G');

$max = 100000000;
$sqrtmax = sqrt($max);
$sieve = array_fill(0, $max+1, true);
$sieve[0] = 0;
$sieve[1] = 0;
for($i = 0; $i <= $sqrtmax; $i++){
  if($sieve[$i]){
    for($j = $i*$i; $j <= $max; $j += $i){
      $sieve[$j] = 0;
    }
  }
}

$primes = [];
$pcount = 0;
for($i = 0; $i <= $max; $i++){
  if($sieve[$i]){
    $primes[$pcount] = $i;
    $pcount++;
  }
}

print($primes[$pcount-1]);
print("\n");

print("end\n");
