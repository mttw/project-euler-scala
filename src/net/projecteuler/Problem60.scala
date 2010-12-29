package net.projecteuler

import Combination._
import SieveOfAtkin._
import GraphAlgorithm._
import Ordering.fromLessThan


object Problem60 {

  
  
  def concatNumbers(x: BigInt*) = BigInt(x.map(_.toString).reduceLeft(_ + _))
  
  def isPrimeConcatable(x: BigInt, y: BigInt, prob: Int = 30): Boolean = concatNumbers(x, y).isProbablePrime(prob) && concatNumbers(y, x).isProbablePrime(prob) 
  def isPrimeConcatable(t: Tuple2[BigInt, BigInt]): Boolean = isPrimeConcatable(t._1, t._2)
  

  def findPrimeConcatableTuples(ps: Seq[BigInt]) = 
    for((p1, p2) <- choose2Different(ps); if(isPrimeConcatable(p1, p2))) yield (p1, p2)
    
  def main(args: Array[String]) = {
	val N = 5
	println("Generating primes...")
	val ps = primes(10000)	

	println("Building graph...")
    val tuples = findPrimeConcatableTuples(ps)
    val edges = tuples.map(t => Edge(t)).toSet
    val graph = Graph(edges)

	printf("Finding cliques of size %d...\n", N)
    val primeSets = findAllCliques(graph, N).map(_.map(_.value))
    val setSumOrdering = fromLessThan[Set[BigInt]](_.sum < _.sum)
    val solution = primeSets.min(setSumOrdering)
    
    printf("The lowest sum of the set of prime-concatable primes with size %d is %s\n", N, solution.sum)
    println("The primes are " + solution.toList.sorted.mkString(", "))
    
  }

}
