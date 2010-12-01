package net.projecteuler

import Partition.partitions

object Problem78 extends Application {
  
  
  /**
   * See Integer Partition (http://en.wikipedia.org/wiki/Integer_partition)
   */
  def solve(mod: Int): Int = partitions.indexWhere(_ % mod == 0) + 1
  
  val mod = 1000000
  printf("The least value of n for which p(n) is divisible by %d is %d.\n", mod, solve(mod))
  
  
}
