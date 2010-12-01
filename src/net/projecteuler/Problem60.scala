package net.projecteuler

import scala.collection.mutable.HashMap
import Sieve._

object Problem60 extends Application {

  val PRIMES = primes(1000)
  
//  def combine2[T](xs: Seq[T]): Seq[Tuple2[T, T]] = 
//    for(i <- 0 until xs.length; j <- i+1 until xs.length) yield(xs(i), xs(j))
  
  def solve(n: Int) = {
    def solve1(pcs: List[List[BigInt]], n: Int): List[List[BigInt]] = {
      if(n == 1) 
        pcs
      else {
//        val pcs2 = for(pc <- pcs; p <- PRIMES; 
//                       if !pc.contains(p) && isPrimeConcatableTo(p, pc)) yield (p :: pc).sortWith(_ < _)
        val pcs2 = (for(pc <- pcs; p <- PRIMES.filter(_ > pc.first); 
                       if isPrimeConcatableTo(p, pc)) yield (p :: pc))
                   .distinct
        println(pcs2.size, pcs2)
        solve1(pcs2, n-1)
      }      
    }
    
    solve1(PRIMES.map(List(_)), n)
  }
	
  def concatNumbers(x: BigInt*) = BigInt(x.map(_.toString).reduceLeft(_ + _))
  
  def isPrimeConcatable(x: BigInt, y: BigInt): Boolean = concatNumbers(x, y).isProbablePrime(30) && concatNumbers(y, x).isProbablePrime(30) 
  
  val cache = new HashMap[Tuple2[BigInt, List[BigInt]], Boolean]

//  def cachedIsPrimeConcatableTo(x: BigInt, ps: List[BigInt]): Boolean = cache.get((x, ps)) match {
//    case Some(v) => print("."); v
//    case None => val v = isPrimeConcatableTo(x, ps); cache += (x, ps) -> v; v 
//  }

  def isPrimeConcatableTo(x: BigInt, ps: List[BigInt]): Boolean = ps match {
    case p :: tail => if(!isPrimeConcatable(x, p)) false else isPrimeConcatableTo(x, tail)  
    case Nil => true
  }
//    for(p <- ps) { 
//      if(!isPrimeConcatable(x, p)) return false
//    }
//    true
//  }
  def isPrimeConcatable(t: Tuple2[BigInt, BigInt]): Boolean = isPrimeConcatable(t._1, t._2)
  
  
//  combine2(PRIMES).filter(isPrimeConcatable).foreach(println)
  println(solve(5))
  
//  printf("The smallest cube for which exactly %d permutations of its digits are cube is %s\n", 
//	     numOfPermutations, solve(numOfPermutations))
}
