
package net.projecteuler

import Numbers.{ints, bigints}
import Calculus.sum

object Problem101 extends Application {
  

  //1. Koeffizient: x**0
  def polyGenFunc(coefficients: Seq[Int]) =
    (x: BigInt) => sum(for((coeff, i) <- coefficients zip ints(0)) yield BigInt(coeff)*x.pow(i))

    
  def polyGenStream(coefficients: Seq[Int], start: BigInt = 0): Stream[BigInt] = {
    def f = polyGenFunc(coefficients)
    def pgs(n: BigInt): Stream[BigInt] = Stream.cons(f(n), pgs(n + 1))
    Stream.cons(f(start), pgs(start + 1))
  }

  def u =  polyGenFunc(Seq(1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1))
  def uStream = polyGenStream(Seq(1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1), 1)
 
  def findFIT(f: BigInt => BigInt, op: BigInt => BigInt): BigInt = {
    for(i <- ints(1)) if(f(i) != op(i)) return op(i)
    null
  }

  def calculateOP(coefficients: Seq[Int]) = None
 
  println(findFIT(u, ((n: BigInt) => n*682-681)))
  println(u(1))
  println(u(2))
  println(u(3))
//  for(i <- 1 until 11) {
//    println(uStream take i toList)
//  }
}
