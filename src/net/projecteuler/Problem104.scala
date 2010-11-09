package net.projecteuler

import Fibonacci.fibonaccis
import Calculus.{sum,product,sumOfDigits, productOfDigits,digits}
import Numbers.bigints

object Problem104 extends Application {
  
  val s = sumOfDigits(BigInt(123456789))
  val p = productOfDigits(BigInt(123456789))
  
  def tailFibonaccis(n : Int): Stream[BigInt] = {
    val modulus = BigInt(10).pow(n)
    def tailFibonaccisTR(prev: BigInt, cur: BigInt): Stream[BigInt] = {
      val next = (prev + cur) % modulus
      Stream.cons(next, tailFibonaccisTR(cur, next))
    }
    Stream.cons(0, Stream.cons(1, tailFibonaccisTR(0, 1)))
  }

  def headFibonaccis(n: Int, prob: Int): Stream[BigInt] = {
    def headFibsTR(prev: BigInt, cur: BigInt): Stream[BigInt] = {
      val next = prev + cur
      val modCur = headDigits(cur, n+prob - (digits(next) - digits(cur)))
      val modNext = headDigits(next, n+prob)
      val headNext = headDigits(modNext, n)
      Stream.cons(headNext, headFibsTR(modCur, modNext))
    }
    Stream.cons(0, Stream.cons(1, headFibsTR(0, 1)))
  }
  
  def headDigits(n: BigInt, num: Int): BigInt = {
    if(digits(n) <= num) n
    else BigInt(n.toString.substring(0, num))
  }
  
  def isPandigital(n: BigInt): Boolean =
    (n.toString.length == 9) && (sumOfDigits(n) == s) && (productOfDigits(n) == p)    
    
    
  for(((i, head), tail) <- (bigints(0) zip headFibonaccis(9, 30) zip tailFibonaccis(9))) {
    if(isPandigital(tail) && isPandigital(head)) {
      printf("The %d-th Fibonacci number is the first for which the first nine digits AND the last nine digits are 1-9 pandigital.\n", i)
      println("Head: " + head)
      println("Tail: " + tail)
      System.exit(0)
    }
  }
  
}
