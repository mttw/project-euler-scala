package net.projecteuler

import scala.collection.mutable.HashMap

object Problem62 extends Application {

  def canonicalPermutation(n: BigInt) = { 
    val orderedDigits = n.toString.map((c: Char) => BigInt(c.toString)).toList.sortWith(_ < _)
    orderedDigits.map(_.toString).reduceLeft(_ + _)
  }
  
  val LIMIT = BigInt(10000)
  
  def solve(numOfPermutations: Int) = {
    var map = new HashMap[String, List[BigInt]]()
    for(i <- BigInt(1) until LIMIT) {
      val cube = i.pow(3)
      val cp = canonicalPermutation(cube)
      
      map.get(cp) match {
	case Some(l) => map += cp -> (cube :: l)
	case None => map += cp -> (cube :: Nil)
      }
    }
    
    val solutions = map.values.filter(_.size == numOfPermutations)
    // return the smallest cube of all solutions
    solutions.map(_.min).min
  }
  
  val numOfPermutations = 5
  printf("The smallest cube for which exactly %d permutations of its digits are cube is %s\n", 
	     numOfPermutations, solve(numOfPermutations))
}
