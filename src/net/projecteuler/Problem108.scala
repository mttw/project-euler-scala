package net.projecteuler

import Numbers.{ints, bigints}
import Sieve.primeStream
import Calculus.factorial
object Problem108 extends Application {
  
  def calcDistinctSolutions(n: BigInt) = {
    var num = 0
    for(x <- BigInt(1) until n+1)
      if((n*(x+n)).mod(x) == 0) num += 1
//    println(n, num)
    num
  }
  
  def calcDistinctSolutions2(n: BigInt) = {
    var num = 0
    for(x <- BigInt(1) until n+1)
      if(n.pow(2).mod(x) == 0) num += 1
//    println(n, num)
    num
  }
  
  def solve(threshold: Int) = {
    bigints(1).findIndexOf((n: BigInt) => calcDistinctSolutions2(n) > threshold) + 1
//    bigints(1).map(_*1).findIndexOf((n: BigInt) => calcDistinctSolutions(n) > threshold) + 1
//    ints(1).map((i: Int) => factorial(i)).findIndexOf((n: BigInt) => calcDistinctSolutions(n) > threshold) + 1
    
  }
  
  val n = solve(300)
  println(n)
  println(calcDistinctSolutions(n))
  
}
