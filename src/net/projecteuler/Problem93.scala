package net.projecteuler

import Combination.choose2


trait Term {
  def eval(): Rational
}

object Value {
  def apply(v: Int) = new Value(v)
}
class Value(v: Int) extends Term {
  def eval() =  Rational(v)
  override def toString() = v.toString
}

object Add {
  def apply(params: Term*) = new Add(params:_*)
}
class Add(params: Term*) extends Term {
  def eval() =  params.map(_.eval).foldLeft(Rational(0))(_+_)
  override def toString() = params.mkString("(", "+", ")")
}

object Mul {
  def apply(params: Term*) = new Mul(params:_*)
}
class Mul(params: Term*) extends Term {
  def eval() =  params.map(_.eval).foldLeft(Rational(1))(_*_)
  override def toString() = params.mkString("(", "*", ")")
}


object Sub {
  def apply(l: Term, r: Term) = new Sub(l, r)
}
class Sub(l: Term, r: Term) extends Term {
  def eval() =  l.eval - r.eval
  override def toString() = "(" + l + "-" + r + ")"
}

object Div{
  def apply(l: Term, r: Term) = new Div(l, r)
}
class Div(l: Term, r: Term) extends Term {
  def eval() =  l.eval / r.eval
  override def toString() = "(" + l + "/" + r + ")"
}


object Problem93 {

  def generateTerms(numbers: Int*) = {
    def genTerms(terms: Seq[Term], operands: List[Value]): Seq[Term] = {
      if(operands.size == 0) terms
      else {
        val l = for(t <- terms; o <- operands)
          yield genTerms(
            Add(t, o) :: Mul(t, o) :: Sub(t, o) :: Sub(o, t) :: Div(t, o) :: Div(o, t) :: Nil,
          operands - o)
        l.flatten
      }
    }
    val operands = numbers.map(Value(_)).toList
    (for(o <- operands) yield genTerms(List(o), operands - o)).flatten
  }

  def findMaxIntSequence(xs: Seq[Int]): Seq[Int] = {
    if(xs.head == 1) 1 :: (xs.zip(xs.tail).takeWhile(t => (t._2 == t._1+1))).map(t => t._2).toList
    else Nil
  }

  def generateSequence(numbers: Int*) =
    generateTerms(numbers:_*).map(_.eval).filter(r => r.isInteger && r.numer > 0).map(_.toInt).distinct.sorted

  def main(args: Array[String]) = {

    val xs = for(a <- 1 until 7; b <- a+1 until 8; c <- b+1 until 9; d <- c+1 until 10)
      yield ((a,b,c,d), generateSequence(a,b,c,d))

    val candidates = for((t, xs) <- xs) yield (t, findMaxIntSequence(xs).size)
    val solution = candidates.toList.max(Ordering.fromLessThan[(Any, Int)](_._2 < _._2))
    println(solution)

  }

}