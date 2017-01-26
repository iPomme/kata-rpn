package io.jorand

import org.scalatest.{Matchers, WordSpecLike}
import RPN._
import cats.data.NonEmptyList
import cats.data.Validated.{invalidNel, valid, invalid}


/**
  * Sample specification.
  */
class RPNSpecs extends WordSpecLike with Matchers {
  "The calculator" should {
    "perform simple calculation" in {
      "1 2 +".calc should equal(valid(3))
    }
    "perform more complex calculation" in {
      "1 2 + 5 * 3 / 5 -".calc should equal(valid(0))
    }

    "return an error if the header is wrong" in {
      "a 2 + 5 * 3 / 5 -".calc should equal(invalidNel(s"a ${RPN.NotNumber}"))
    }

    "return multiple errors if the header, operator and numbers are wrong" in {
      "a 2 + 5 * 3 : bb -".calc should equal(invalid(NonEmptyList.of(s"a ${RPN.NotNumber}",s": ${RPN.NotOperator}",s"bb ${RPN.NotNumber}")))
    }
  }
}
