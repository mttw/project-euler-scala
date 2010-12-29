package net.projecteuler

import Calculus.{sum, digits}
import Ordering.fromLessThan
import Combination.choose

object MagicNgon {

  // from example: MagicNgon( (4,3) :: (6,2) :: (5,1) :: Nil )
  def apply(tuples: Seq[Tuple2[Int, Int]]) = new MagicNgon(tuples)

  // from example: MagicNgon( List(4,6,5), List(3,2,1) )
  def apply(outer: Seq[Int], inner: Seq[Int]) = {
    assert(outer.size == inner.size)
    val tuples = outer.zip(inner)
    new MagicNgon(tuples)
  }
}

class MagicNgon(tps: Seq[Tuple2[Int, Int]]) extends Ordered[MagicNgon] {
  private val lowestIndex = tps.zipWithIndex.min(fromLessThan[Tuple2[Tuple2[Int, Int], Int]](_._1._1 < _._1._1))._2

  // canocial tuple
  val tuples = tps.slice(lowestIndex, tps.size) ++ tps.slice(0, lowestIndex)

  // representation according to problem description
  val repr = tuples.zip(tuples.tail).map(t => List(t._1._1, t._1._2, t._2._2)) ++ List(List(tuples.last._1, tuples.last._2, tuples.head._2))

  def isMagic = (repr.map(sum).distinct.size == 1)

  def magicSum = sum(repr.head)

  def toBigInt() = BigInt(repr.map(_.mkString("")).mkString(""))

  override def toString() = repr.map(_.mkString(",")).mkString("; ")

  override def equals(obj: Any) = obj match {
    case m: MagicNgon => (repr == m.repr)
    case _ => false
  }

  override def compare(that: MagicNgon): Int =  this.toBigInt.compare(that.toBigInt)

}



object Problem68 {

  def main(args: Array[String]) = {
    val N = 10
    val range = (1 until N+1).toList
    val magicNgons = (for(outer <- choose(N/2, range).filter(xs => xs.forall(e => xs.head >= e)); inner <- choose(N/2, range.diff(outer));
                           mng = MagicNgon(outer, inner); if(mng.isMagic)) yield mng).distinct.sorted
    val solution = magicNgons.filter(m => digits(m.toBigInt) < 17).last
    println(solution, solution.toBigInt)

  }
}