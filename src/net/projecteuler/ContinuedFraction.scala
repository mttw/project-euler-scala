package net.projecteuler

import scala.math.max
import Numbers.ints
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

object ContinuedFraction {
  def apply(start: List[Int], period: List[Int]) = new ContinuedFraction(start, period)

  def fromSqrt(n: BigInt): ContinuedFraction = {
    var previous = (Irrational(0), Irrational(0, 1, n))
    val xs = ListBuffer[Int]()
    val indexMap = new HashMap[Irrational, Int]

    for(i <- ints(0)) {
      val f = previous._2.toInt
      if(f==0) return ContinuedFraction(xs.toList, Nil)
      previous = (Irrational(f), (previous._2 - Irrational(f, 0, n)).invert)
      xs.append(f)
      if(indexMap contains previous._2) {
        val index = indexMap(previous._2)
        val xs2 = xs.toList.splitAt(index+1)
        return ContinuedFraction(xs2._1, xs2._2)
      }
      indexMap += previous._2 -> i

    }
    return null
  }
}

class ContinuedFraction(s: List[Int], p: List[Int] = Nil) {

  val start = s
  val period = p

  def span(length: Int) =
    start.take(length) ::: (if(period.size > 0) List.tabulate(length - start.size)(i => period(i%period.size)) else Nil)

  def toRational(limit: Int = 1) = {
    span(limit+1).map(Rational(_)).reverse.foldLeft(Rational(0))(_.invert + _)
  }

  def toRationalSequence(): Stream[Rational] = {
    def toRS(limit: Int): Stream[Rational] =
      if(period.size == 0 && limit >= start.size) Stream.empty
      else Stream.cons(toRational(limit), toRS(limit+1))
    toRS(1)
  }

  override def toString() = "[" +  start.mkString(",") + "," + period.mkString("(", ",", ")") + "]"

  override def equals(obj: Any) = obj match {
    case cf: ContinuedFraction => (start == cf.start) && (period==cf.period)
    case _ => false
  }

}
