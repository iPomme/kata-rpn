package io.jorand

import org.scalatest.{Matchers, WordSpecLike}
import cats.data.Validated.{valid, invalid}
import cats.data.NonEmptyList
import RPN._


/**
  * Sample specification.
  */
class RPNSpecs extends WordSpecLike with Matchers {
  "The claculator engine" should {
    "perform simple calculation" in {
      "1 2 +".calc should equal(valid(3))
    }
    "perform complex calculation" in {
      "1 2 + 5 * 3 / 5 -".calc should equal(valid(0))
    }
    "return one error when error in the header" in {
      "a 2 + 5 * 3 / 5 -".calc should equal(invalid(s"a ${RPN.NOTNUMBER}"))
    }
  "return multiple errors when error in the header, the operator and numbers" in {
      "a 2 : 5 * 3 / bb -".calc should equal(invalid(NonEmptyList.of(s"a ${RPN.NOTNUMBER}", s": ${RPN.NOTOPERATOR}"),s"bb ${RPN.NOTNUMBER}"))
    }
  }
}
