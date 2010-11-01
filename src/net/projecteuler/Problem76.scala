package net.projecteuler

import scala.collection.mutable.HashMap

object Problem76 extends Application {

  def findSums(n: Int) = {
    def writeAsSums(n: Int, numOfAddends: Int, prevAppend: Int = 1): List[List[Int]] = {
      if(numOfAddends == 1) List(List(n))
      else {
        for(i <- List.range(prevAppend,n/numOfAddends + 1); addends <- writeAsSums(n-i, numOfAddends-1, i)) yield i :: addends
      }
    }
    (for(i <- 2 until n + 1) yield writeAsSums(n, i)).flatten
  }  
  
  
  def solve(n: Int): BigInt = {
    val numOfSums = new HashMap[Int, BigInt]
    
    for(i <- 1 until n; j <- i until n+1) {
      numOfSums += j -> (numOfSums.getOrElse(j-i, BigInt(1)) + numOfSums.getOrElse(j, BigInt(0)))
    }
    numOfSums(n)
  }
  
  val n = 100
  printf("There are %s different ways to write %d as " +
      "a sum of at least two positive integers\n", solve(n), n)
}
