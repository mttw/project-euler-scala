package net.projecteuler

import org.specs.Specification
import scala.math.sqrt

/**
 * Created by IntelliJ IDEA.
 * User: wurm.matthias
 * Date: 20.12.10
 * Time: 13:59
 * To change this Xtemplate use File | Settings | File Templates.
 */

class ContinuedFractionSpec extends Specification {



  "[1;2,3] spanned to length 8 is still [1;2,3]" in {
    new ContinuedFraction(List(1,2,3)).span(8) must_== List(1,2,3)
  }

  "[1;2,3,(4,5)] spanned to length 8 is [1;2,3,4,5,4,5,4]" in {
    new ContinuedFraction(List(1,2,3), List(4,5)).span(8) must_== List(1,2,3,4,5,4,5,4)
  }

  "[1;(2)] toRational with length 2 is 3/2" in {
      new ContinuedFraction(List(1), List(2)).toRational(2) must_== Rational(3,2)
    }

  "[1;(2)] toRational with length 5 is 41/29" in {
      new ContinuedFraction(List(1), List(2)).toRational(5) must_== Rational(41,29)
    }

  "[1;(2)] toRationalSequence is 1 ... 41/29" in {
      new ContinuedFraction(List(1), List(2)).toRationalSequence.take(5).toList must_==
        List(Rational(1), Rational(3,2), Rational(7,5), Rational(17,12), Rational(41,29))
    }

  "fromSqrt 2 is [1;(2)]" in {
      ContinuedFraction.fromSqrt(2) must_== ContinuedFraction(List(1), List(2))
    }

  "fromSqrt 3 is [1;(1,2)]" in {
      ContinuedFraction.fromSqrt(3) must_== ContinuedFraction(List(1), List(1,2))
    }
  "fromSqrt 5 is [2;(4)]" in {
      ContinuedFraction.fromSqrt(5) must_== ContinuedFraction(List(2), List(4))
    }
  "fromSqrt 6 is [2;(2,4)]" in {
      ContinuedFraction.fromSqrt(6) must_== ContinuedFraction(List(2), List(2,4))
    }
  "fromSqrt 7 is [2;(1,1,1,4)]" in {
      ContinuedFraction.fromSqrt(7) must_== ContinuedFraction(List(2), List(1,1,1,4))
    }

  "fromSqrt 8..13" in {
      ContinuedFraction.fromSqrt(8) must_== ContinuedFraction(List(2), List(1,4))
      ContinuedFraction.fromSqrt(10) must_== ContinuedFraction(List(3), List(6))
      ContinuedFraction.fromSqrt(11) must_== ContinuedFraction(List(3), List(3,6))
      ContinuedFraction.fromSqrt(12) must_== ContinuedFraction(List(3), List(2,6))
      ContinuedFraction.fromSqrt(13) must_== ContinuedFraction(List(3), List(1,1,1,1,6))

    }


}