package net.projecteuler

import java.math.{MathContext=>jMC}

object Rational {
  def apply(n: BigInt, d: BigInt = 1) = new Rational(n, d)
}

class Rational(n: BigInt, d: BigInt = 1) extends Ordered[Rational] {
//  private def gcd(x: Int, y: Int): Int = {
//    if (x == 0) y
//    else if (x < 0) gcd(-x, y)
//    else if (y < 0) -gcd(x, -y)
//    else gcd(y % x, x)
//  }
  private val g = n.gcd(d)
  
  val numer: BigInt = n/g
  val denom: BigInt = d/g

  def toBigInt() = n/d
  
  def +(that: Rational) =
    new Rational(numer * that.denom + that.numer * denom,
      denom * that.denom)
  def -(that: Rational) =
    new Rational(numer * that.denom - that.numer * denom,
      denom * that.denom)
  def *(that: Rational) =
    new Rational(numer * that.numer, denom * that.denom)
  def /(that: Rational) =
    new Rational(numer * that.denom, denom * that.numer)
  
  override def toString(): String =  numer.toString + (if(denom != 1) "/" + denom else "");
  override def compare(that: Rational): Int =  (numer*that.denom).compare(that.numer*denom)

}


object Binomial {
  import Calculus.factorial
  import Calculus.product
  
  def binomialCoefficient(n:Int, k:Int) = factorial(n) / (factorial(k) * factorial(n-k))
  def binCoeff(n: Int, k: Int): BigInt = product(for(i <- n-k+1 until n+1) yield BigInt(i)) / factorial(k) 

}

object Numbers {
  def bigints(n: BigInt): Stream[BigInt] = Stream.cons(n, bigints(n+1))
  def ints(n: Int): Stream[Int] = Stream.cons(n, ints(n+1))
  
  def abs(x: Rational): Rational = if(x.numer*x.denom >= 0) x else new Rational(0) - x
  def abs(x: BigDecimal): BigDecimal = x.abs

  def isqrt(x: BigInt): BigInt = {
    val xd = BigDecimal(x, jMC.DECIMAL128)
    def square(a: BigDecimal) =  a*a
    def sqrtIter(guess: BigDecimal): BigInt =
      if (isGoodEnough(guess)) guess.toBigInt
      else sqrtIter(improve(guess))
    def improve(guess: BigDecimal): BigDecimal = (guess + xd / guess) / 2
    def isGoodEnough(guess: BigDecimal) = abs(square(guess) - xd) < 1
      
    sqrtIter(BigDecimal(1, jMC.DECIMAL128))
  }
  
  
}

object Partition {
  
  /**
   * Stream of Partitions of n beginning from 1.
   * 
   * Algorithm from: http://www.numericana.com/answer/numbers.htm#partitions
   */
def partitions(): Stream[BigInt] = {
    def partitionsTR(i: Int, P: Map[Int, BigInt]): Stream[BigInt] = { 
      var (j, k, s) = (1, 1, BigInt(0))
      while(j > 0) {
        j = i - (3*k*k + k)/2
        if(j >= 0) s -= BigInt(-1).pow(k)*P(j)
        
        j = i - (3*k*k - k)/2
        if(j >= 0) s -= BigInt(-1).pow(k)*P(j)
        
        k += 1
      }
      Stream.cons(s, partitionsTR(i+1, P + (i -> s)))
    }
    partitionsTR(1, Map[Int, BigInt](0 -> BigInt(1)))
  }    
  /**
   * Partition Function.
   * 
   * See http://en.wikipedia.org/wiki/Integer_partition on Integer Partitions.
   *  
   * @param m The number to be partitioned
   * @return The number of paritions of <code>m</code>
   */
  def partition(n: Int): BigInt = if(n <= 0) 0 else partitions.apply(n-1)  
}

object Calculus {
  def sum(xs: Seq[Int]): Int = (0 /: xs) (_ + _)
  def sum(xs: Seq[BigInt]): BigInt = (BigInt(0) /: xs) (_ + _)

  def product(xs: Seq[Int]): Int = (1 /: xs) (_ * _)
  def product(xs: Seq[BigInt]): BigInt = (BigInt(1) /: xs) (_ * _)

  def digits(x: BigInt) = x.toString.size

  def sumOfDigits(x: BigInt) = 
	  x.toString.map((x: Char) => BigInt(x.toString)).foldLeft(BigInt(0))(_ + _)

  def productOfDigits(x: BigInt) = 
    x.toString.map((x: Char) => BigInt(x.toString)).foldLeft(BigInt(1))(_ * _)
	  
  def factorial(n: BigInt): BigInt = 
	  if(n > 1) n * factorial(n-1) 
	  else 1 
}



object Fibonacci {
	
  def fib(n: Int) = {
    def fibTR(n: Int, b: Int, a: Int): Int = n match {
      case 0 => a 
      case _ => fibTR(n - 1, a + b, b)
    }
	fibTR(n, 1, 0) 
  }

  def fibList(limit: Int): List[Int] = {
	  def fibListTR(limit: Int, xs: List[Int]): List[Int] = {
	 	  // TODO wurm.matthias: Find solution to access last 2 elements (so the final reverse can be removed)
	 	  val a :: b :: _ = xs
	 	  if(a+b > limit) xs else fibListTR(limit, a+b :: xs)
	  }
	  fibListTR(limit, List(1, 0)).reverse
  }

  def fibonaccis(): Stream[BigInt] = {
    def fibonaccisTR(prev: BigInt, current: BigInt): Stream[BigInt] = {
      val next = prev + current
      Stream.cons(next, fibonaccisTR(current, next))
    }
    Stream.cons(0, Stream.cons(1, fibonaccisTR(0, 1))) 
  }
  
  
}


