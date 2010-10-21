package net.projecteuler

object Problem6 extends Application {

  def abs(x: Int) = if(x >= 0) x else -x
  def sum(xs: List[Int]) = (0 /: xs)(_ + _)
  def square(x: Int) = x * x
	
  def solve(xs: List[Int]) =
	  abs(sum(xs.map(square)) - square(sum(xs)))
	
  printf("The difference between the sum of the squares of the first one hundred natural numbers and the square of the sum" 
      + " is %d", solve(List.range(1,101)))
 
}
