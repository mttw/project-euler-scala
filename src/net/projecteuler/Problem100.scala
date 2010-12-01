package net.projecteuler

import Numbers.bigints
//import Rational

object Problem100 extends Application {
  
  // after some math, r must be at least 707106781187
  // see Wolfram Alpha: Roots of 8x²-8x+1-(2*10^(12)-1)²
  // then: ceil(1/2 (1+sqrt(1999999999998000000000001)))
  
//  val rMin = BigInt("707106781187")
  val rMin = BigInt(7071067L)
  
  def calc(r: BigInt) = BigInt(8)*r*(r-1) + 1
  
  
//  def abs(x: BigDecimal) = if(x >= 0) x else -x

//  def isqrt2(x: Double) = {
//    def square(x: Double) =  x*x
//    def sqrtIter(guess: Double): Double =
//      if (isGoodEnough(guess)) guess
//      else sqrtIter(improve(guess))
//    def improve(guess: Double) =
//      (guess + x / guess) / 2
//    def isGoodEnough(guess: Double) =
//      Math.abs(square(guess) - x) < 0.001
//    sqrtIter(1.0)
//  }
  
//  def isqrt(x: BigInt): BigInt = {
//    val xd = x.toDouble
//    def square(a: Double) =  a*a
//    def sqrtIter(guess: Double): BigInt =
//      if (isGoodEnough(guess)) BigInt(guess.toLong)
//      else sqrtIter(improve(guess))
//    def improve(guess: Double) = {
////      println(guess)
//      (guess + xd / guess) / 2
//      }
//    def isGoodEnough(guess: Double) =
//      Math.abs(square(guess) - xd) < 1
//      
//    sqrtIter(1.0)
//  }

  def abs(x: Rational) = if(x.numer*x.denom >= 0) x else new Rational(0) - x
  
  def isqrt(x: BigInt): BigInt = {
    val xd = new Rational(x, 1)
    def square(a: Rational) =  a*a
    def sqrtIter(guess: Rational): BigInt =
      if (isGoodEnough(guess)) guess.toBigInt
      else sqrtIter(improve(guess))
    def improve(guess: Rational) = {
//      println(guess)
      val i = (guess + xd / guess) / new Rational(2)
      println(">" + i)
      i
      }
    def isGoodEnough(guess: Rational) =
      abs(square(guess) - xd) < new Rational(1)
      
    sqrtIter(new Rational(1))
  }
  
  
  def isSquare(x: BigInt): Boolean = (x == isqrt(x).pow(2))
  println((new Rational(3,4) /  new Rational(2)).toBigInt)
  println(isqrt(2))
  println(isqrt(4))
  println(isqrt(9))
  println(isqrt(16))
  println(isqrt(25))
  println(isqrt(36))
  println(isqrt(49))
//  val x = BigInt(2222707106781187L)
//  val y = BigInt(2222707106781187L).pow(2)
//  val z = isqrt(y)
//  println(x)
//  println(y)
//  println(z)
//  println(z.pow(2))
//  println(isSquare(y))
//  println(isSquare(y+1))
  // begin from rMin, check if 8r^2 - 8r + 1 is square
  
//  println(new Rational(4,8))
//  println(new Rational(4,4))
  for((i, r) <- bigints(0) zip bigints(rMin)) {
//    if(i % 100 == 0) {
      println(i)
//    }
    val rx = calc(r)
    println(rx)
    println(isqrt(rx))
    if(isSquare(rx)) {
      println(r)
      System.exit(0)
    }
    
  }
}
