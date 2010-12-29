package net.projecteuler

import Numbers.isSquare

object Problem66 {

  def isDiophantineSolution(D: BigInt)(x: BigInt, y: BigInt) = x.pow(2) - y.pow(2)*D == 1

  def findDiophantineSolution(D: BigInt): Tuple2[BigInt, BigInt] = {
    val cf = ContinuedFraction.fromSqrt(D)
    val r = cf.toRationalSequence.find(rat => isDiophantineSolution(D)(rat.numer, rat.denom)).get
    (r.numer, r.denom)
  }

  def main(args: Array[String]) = {
    val limit = 1000
    val l = for(d <- 2 until limit+1; if(!isSquare(d))) yield (d,findDiophantineSolution(d))
    val solution = l.max(Ordering.fromLessThan[(Any, (BigInt, BigInt))](_._2._1 < _._2._1))
    printf("The value of D<=%d in minimal solutions of x for which the largest value of x is obtained is %d.\n", limit, solution._1)
  }

}