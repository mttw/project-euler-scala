package net.projecteuler

import Partition.partition

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
  
  
  /**
   * See Integer Partition (http://en.wikipedia.org/wiki/Integer_partition)
   */
  def solve(n: Int): BigInt = partition(n) - 1
  
  val n = 100
  printf("There are %s different ways to write %d as " +
      "a sum of at least two positive integers\n", solve(n), n)
}
