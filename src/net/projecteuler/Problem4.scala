package net.projecteuler


object Problem4 extends Application {

  def isPalindrome(x: Int) = 
	  x.toString.reverse == x.toString

  def generateProducts(start: Int, end: Int): Seq[Int] = 
	  for(i <- start until end + 1; j <- i until end + 1) yield i*j 

  def solve(start: Int, end: Int): Int = 
	  generateProducts(start, end).filter(isPalindrome _).max

  printf("The largest palindrome made from the product of two 3-digit numbers" 
      + " is %d", solve(100, 1000)) 
}
