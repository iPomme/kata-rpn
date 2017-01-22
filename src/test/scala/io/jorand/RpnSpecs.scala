package io.jorand

import org.scalatest.{Matchers, WordSpecLike}
import cats.data.Validated.{valid, invalid}
import cats.data.NonEmptyList
import RPN._


/**
  * Sample specification.
  */
class RPNSpecs extends WordSpecLike with Matchers {
  "The RPN Calculator" should {
    "perform simple calculation" in {
      "1 2 +".calc should equal(valid(3))
    }
    "perform complex calculation" in {
      "1 2 + 5 * 3 / 5 -".calc should equal(valid(0))
    }
    "return an error if the header is wrong" in {
      "a 2 + 5 * 3 / 5 -".calc should equal(invalid(NonEmptyList.of(s"a ${RPN.NotValidNumber}")))
    }
    "return multiple errors if the header is wrong, an operator is wrong and a number is wrong" in {
      "a 2 + 5 : 3 / bb -".calc should equal(invalid(NonEmptyList.of(s"a ${RPN.NotValidNumber}", s": ${RPN.NotValidOperator}", s"bb ${RPN.NotValidNumber}")))
    }


  }
}
