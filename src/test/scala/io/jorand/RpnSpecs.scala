package io.jorand


import RPN._
import org.scalactic.{Bad, Good, Many, One}
import org.scalatest.{Matchers, WordSpecLike}


/**
  * Sample specification.
  */
class RPNSpecs extends WordSpecLike with Matchers {
  "The RPN calculator" should {
    "perform simple calculation" in {
      "1 2 +".calc should equal(Good(3))
    }

    "perform more complicated calculation" in {
      "1 2 + 5 * 3 /".calc should equal(Good(5))
    }

    "return an error if header not correct" in {
      "a 2 +".calc should equal(Bad(One(s"a ${RPN.NotNumber}")))
    }

    "return multiple errors if mutliple errors in the stack" in {
      "1 2 + d :".calc should equal(Bad(Many(s"d ${RPN.NotNumber}",s": ${RPN.NotOperand}")))
    }
  }
}
