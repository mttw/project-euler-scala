package net.projecteuler

import Numbers.bigints
import scala.io.Source

object Problem102 extends Application {
  
  
  
  def isSameSide(p1: Tuple2[BigInt, BigInt], p2: Tuple2[BigInt, BigInt])(p3: Tuple2[BigInt, BigInt], p4: Tuple2[BigInt, BigInt]) = {
    if(p1._1 == p2._1) {
      (p3._1 - p1._1)*(p4._1 - p1._1) >= 0
    } else {
    
      val a = Rational(p1._2 - p2._2, p1._1 - p2._1)
      val b = Rational(p1._2) - a * Rational(p1._1)
      def f(x: Rational) = a*x + b
    
      (Rational(p3._2) - f(Rational(p3._1)))*(Rational(p4._2) - f(Rational(p4._1))) >= Rational(0)
    }
  }
  
  def isPointInTriangle(p1: Tuple2[BigInt, BigInt], p2: Tuple2[BigInt, BigInt], p3: Tuple2[BigInt, BigInt])(p: Tuple2[BigInt, BigInt]) =
    isSameSide(p1, p2)(p, p3) && isSameSide(p1, p3)(p, p2) && isSameSide(p2, p3)(p, p1)

  def isOriginInTriangle(t: Tuple3[Tuple2[BigInt, BigInt], Tuple2[BigInt, BigInt], Tuple2[BigInt, BigInt]]): Boolean =
    isOriginInTriangle(t._1, t._2, t._3)
    
  def isOriginInTriangle(p1: Tuple2[BigInt, BigInt], p2: Tuple2[BigInt, BigInt], p3: Tuple2[BigInt, BigInt]): Boolean =
    isPointInTriangle(p1, p2, p3)((0, 0))

  def lineToPointList(line: String) = { 
    val l = (for(group <- line.split(",").grouped(2))
      yield Tuple2(BigInt(group(0)), BigInt(group(1)))).toList
    (l(0), l(1), l(2))
  }
    
  
  def solve() = 
    Source.fromFile("triangles.txt")
      .getLines
      .map(lineToPointList)
      .filter(isOriginInTriangle(_)).size    
    

  printf("The number of triangles for which the interior contains the origin is %d.\n", solve)
  
}
