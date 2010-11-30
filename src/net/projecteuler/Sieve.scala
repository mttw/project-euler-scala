package net.projecteuler


import scala.collection.mutable.HashMap
import Numbers.isqrt

object SieveOfAtkins {

  def primes(limit: BigInt): IndexedSeq[BigInt] = {
    val is_prime = HashMap[BigInt, Boolean]()
    val L = isqrt(limit)
    for(x <- BigInt(1) to L+1; y <- BigInt(1) to L+1) {
      val (xPow2, yPow2) = (x.pow(2), y.pow(2))
      
      var n = xPow2*4+yPow2
      if(n <= limit && (n%12==1 || n%12==5))
        is_prime += n -> !is_prime.getOrElse(n, false)
      n = xPow2*3+yPow2
      if(n <= limit && n%12==7)
        is_prime += n -> !is_prime.getOrElse(n, false)
      n = xPow2*3-yPow2
      if(x > y && n <= limit && n%12==11)
        is_prime += n -> !is_prime.getOrElse(n, false)
    }
    for(n <- BigInt(5) until L+1; if is_prime.getOrElse(n, false)) {
      val nSquare = n.pow(2)
      for(k <- BigInt(1) until limit/nSquare + 1)
        is_prime += nSquare*k -> false
    }
    IndexedSeq(BigInt(2),BigInt(3)) ++ (for(n <- BigInt(5) until limit+1; if is_prime.getOrElse(n, false)) yield n)
  }

}


object Sieve {

  
  def primes2(n: Int): List[Int] = {
          def nomults(s: Int, xs: Seq[Int]): List[Int] = (for (x <- xs if x % s != 0) yield x).toList
          def sieve(xs: List[Int]): List[Int] = {
                  if (xs.isEmpty) xs
                  else {
                          val p = xs.first
                          val nxs = nomults(p, xs drop 1)
                          p :: sieve(nxs)
                  }
          }
  
          val odds = nomults(2, 2 to n)
          2 :: sieve(odds)
  }
  

//  private def primeStream(nums: Stream[BigInt]): Stream[BigInt] =
//    Stream.cons(nums.head, primeStream ((nums tail) filter (x => x % nums.head != 0)) )

  def primeStream: Stream[BigInt] = {
    def primeStream1(nums: Stream[BigInt]): Stream[BigInt] =
      Stream.cons(nums.head, primeStream1 ((nums tail) filter (x => x % nums.head != 0)) )

    primeStream1(Numbers.bigints(2))
  }

  def primes(n: Int) = primeStream take n toList
}


