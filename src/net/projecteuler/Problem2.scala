package net.projecteuler

import Fibonacci._

object Problem2 extends Application {

  def isEven(x: Int) = (x % 2 == 0)

  def sum(xs: List[Int]): Int = (0 /: xs) (_ + _)
  
  def solve(limit: Int): Int = { 
	sum(fibList(limit).filter(isEven))
  }

  
  val limit = 4000000
  printf("The sum of all the even-valued terms in the Fibonacci sequence " +
        "which do not exceed %d is %d", limit, solve(limit))
}
