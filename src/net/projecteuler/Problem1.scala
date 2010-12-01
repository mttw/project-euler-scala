package net.projecteuler

object Problem1 extends Application {

  def modN(n: Int)(x: Int) = ((x % n) == 0)

  def sum(xs: List[Int]): Int = (0 /: xs) (_ + _)
  def sum2(xs: List[Int]): Int = xs.foldLeft(0)(_ + _)
  def sum3(xs: List[Int]): Int = xs.foldLeft(0)((x: Int, y: Int) => x+y)

  def solve(list: List[Int]): Int = 
	sum(list.filter((x: Int) => modN(3)(x) || modN(5)(x)))
  
	

  val ceiling = 1000
  printf("The sum of all the multiples of 3 or 5 below %d is %d", 
    ceiling, solve(List.range(1, ceiling)))
}
