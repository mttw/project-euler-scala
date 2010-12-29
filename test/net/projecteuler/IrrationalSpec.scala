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

class IrrationalSpec extends Specification {



  "1 to double is 1.0" in {
    Irrational(1).toDouble must_== 1.0
  }

  "1+sqrt(2) to double is 2.141" in {
    Irrational(1, 1, 2).toDouble must_== 1.0 + sqrt(2.0)
  }


  /*"1 to string is '1'" in {
    Irrational(1).toString must_== "1"
  }
*/
  "1+sqrt(2) to string is '1 + 1*sqrt(2)'" in {
    Irrational(1, 1, 2).toString must be equalTo("1 + 1*sqrt(2)")
  }

  "1+sqrt(2) equals" in {
    Irrational(1, 1, 2).equals(Irrational(1, 1, 2)) must be equalTo true
  }


  "1+sqrt(2) conjugated is 1 - sqrt(2)" in {
    Irrational(1, 1, 2).conjugate must be equalTo Irrational(1, -1, 2)
  }

  "1+sqrt(2) inverted is -1 + sqrt(2)" in {
    Irrational(1, 1, 2).invert must be equalTo Irrational(-1, 1, 2)
  }

  "1+sqrt(2) < 1 + sqrt(3)" in {
    Irrational(1, 1, 2).invert must be equalTo Irrational(-1, 1, 2)
  }

/*
  "1+sqrt(4) to string 3" in {
    Irrational(Rational(1), Rational(1), 4).toString must be equalTo("3")
  }
*/

}