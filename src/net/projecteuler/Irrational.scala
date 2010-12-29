package net.projecteuler

import scala.math.sqrt
import Numbers.{isSquare, isqrt}

object Irrational {
  def apply(p: Rational, q: Rational, adjungate: BigInt): Irrational = {
    // TODO alle Quadrate aus adjungate entfernen
    new Irrational(p, q, adjungate)
  }
  def apply(p: BigInt, q: BigInt, adjungate: BigInt): Irrational =
    Irrational(Rational(p), Rational(q), adjungate)

  def apply(p: BigInt): Irrational =
    Irrational(Rational(p), Rational(0), 1)

}

class Irrational(x: Rational, y: Rational, a: BigInt = 1) extends Ordered[Irrational]{

  val p = x
  val q = y
  val adjungate = a

  def toDouble() = p.toDouble + q.toDouble * sqrt(adjungate.toDouble)

  def toInt() = toDouble.toInt

  def conjugate = new Irrational(p, Rational(0)-q, adjungate)

  def invert = {
    val c = this.conjugate
    val r = this * c
    new Irrational(c.p/r.p, c.q/r.p, adjungate)
  }

  def +(that: Irrational) = {
    assert(adjungate == that.adjungate)
    new Irrational(p + that.p, q + that.q, adjungate)
  }
  def -(that: Irrational) = {
    assert(adjungate == that.adjungate)
    new Irrational(p - that.p, q - that.q, adjungate)
  }
  def *(that: Irrational) = {
    assert(adjungate == that.adjungate)
    new Irrational(p * that.p + q * that.q * Rational(adjungate), p * that.q + q * that.p, adjungate)
  }
  def /(that: Irrational) = this * that.invert

  override def toString(): String =  "" + p + " + " + q + "*sqrt(" + adjungate + ")"

  override def equals(obj: Any) = obj match {
    case i: Irrational => (p==i.p) && (q==i.q) && (adjungate==i.adjungate)
    case _ => false
  }

  override def compare(that: Irrational): Int =  this.toDouble.compare(that.toDouble)

  override def hashCode = p.hashCode + q.hashCode * adjungate.hashCode
}

