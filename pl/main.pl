print("start\n");

my $max = 100000000;
my $sqrtmax = sqrt($max);
my @sieve = (1) x ($max+1);
$sieve[0] = 0;
$sieve[1] = 0;
for($i = 0; $i <= $sqrtmax; $i++){
  if($sieve[$i]){
    for($j = $i*$i; $j <= $max; $j += $i){
      $sieve[$j] = 0;
    }
  }
}

my @primes;
my $pcount = 0;
for($i = 0; $i <= $max; $i++){
  if($sieve[$i]){
    @primes[$pcount] = $i;
    $pcount++;
  }
}

print($primes[$pcount-1]);
print("\n");

print("end\n");
