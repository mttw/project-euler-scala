package net.projecteuler

import scala.testing.SUnit._
import Sieve._

object Problem77 extends Application {
  
  val limit = 5000
  val PRIMES = primeStream takeWhile(_ <= limit) toIndexedSeq
  
  def isPrime(n: Int) = PRIMES contains n

  def ints(n: Int): Stream[Int] = Stream.cons(n, ints(n+1))
  def ints(): Stream[Int] = ints(1)

  def writeAsSums(n: Int, numOfAddends: Int, prevAppend: Int = 1): List[List[Int]] = {
    if(numOfAddends == 1) {
      if(isPrime(n)) List(List(n))
      else List(Nil)
    }
    else {
      for(p <- List.range(prevAppend,n/numOfAddends + 1).filter(isPrime); addends <- writeAsSums(n-p, numOfAddends-1, p);
        if addends.size > 0)
        yield p :: addends
    }
  }
  
  def findAllSums(n: Int) = (for(i <- 2 until n + 1) yield writeAsSums(n, i)).flatten
  

  
  def solve(limit: Int): Int = (ints map findAllSums).indexWhere(_.size > limit) + 1
  
//  def solve2(limit: Int): Int = {
//    for(i <- ints(10)) {
//      val sums = findAllSums(i)
//      println(i, sums.size)
//      if(sums.size > limit) return i
//    }
//    throw new RuntimeException("No solution found")
//  }

  printf("The first value which can be written as the sum of primes in over %d different ways is %d\n", limit, solve(limit))

}
