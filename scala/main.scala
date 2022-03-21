import scala.math.sqrt

object prime {
  def main(args: Array[String]) : Unit = {
    println("start")
    val max = 100000000
    val sqrtmax = sqrt(max).toInt
    var sieve = Array.fill[Boolean](max+1)(true)
    sieve(0) = false
    sieve(1) = false
    for (i <- 2 to sqrtmax){
      if(sieve(i)){
        for (j <- i*i to max by i){
          sieve(j) = false
        }
      }
    }

    var primes = new Array[Int](max+1) 
    var pcount = 0
    for (i <- 2 to max){
      if(sieve(i)){
        primes(pcount) = i
        pcount += 1
      }
    }

    println("found " + pcount.toString() + " primes")
    println(primes(pcount-1))

    println("end")
  }
}