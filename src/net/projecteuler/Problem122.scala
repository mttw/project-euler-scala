package net.projecteuler

import scala.collection.mutable.HashMap
import Calculus.sum
object Problem122 extends Application {
  
  val S = HashMap[Int, List[List[Int]]]()
  
  def findMin(xss: List[List[Int]]): List[List[Int]] = {
    val min = xss.map(_.size).min
    xss.filter(_.size == min).toList
  }
  
  def merge(l1: List[List[Int]], l2: List[List[Int]], n: Int): List[List[Int]] = { 
    (for(e1 <- l1; e2 <- l2) yield (e1 ::: e2 ::: List(n)).distinct).toList
  }
  
  
  
  def s(n: Int): List[List[Int]] = {
    if(n == 1) {
      S += n -> List(List())
    }
    else {
      if(!(S contains n)) {
        val lst = for(i <- 1 until (n / 2) + 1) yield merge(s(i), s(n-i), n)
        
        S += n -> findMin(lst.toList.flatten).distinct
        printf("%d/%d\n", n, N)
      }
    }
    
    S(n)
      
  }
  
  def solve(limit: Int) = {
    s(limit)
    sum((2 until limit+1).map(S(_).head.size)) 
  }

  
  val N = 200
  
  printf("For m(k) as the minimum number of multiplications to compute n^(k), the sum of m(k) for 1..%d is %d\n", N, solve(N))
  
  
  
}
