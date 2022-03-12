let main = 
  Printf.printf "start\n";;

  let max = 100000000 in
  let sqrtmax = truncate (sqrt (float_of_int max))
  and sieve = Array.make (max+1) true in

  sieve.(0) <- false;
  sieve.(1) <- false;
  for i = 2 to sqrtmax do
    if sieve.(i) then
      for j = i to Int.div max i do
        sieve.(j*i) <- false;
        ()
      done
    else ()
  done;


  let pcount = ref 0
  and primes = Array.make (max+1) 0 in
  for i = 0 to max do
    if sieve.(i) then begin
      primes.(!pcount) <- i;
      pcount := !pcount+1;
      ()
    end else ()
  done;

  Printf.printf "%d\n" primes.(!pcount - 1);;

  Printf.printf "end\n";;
