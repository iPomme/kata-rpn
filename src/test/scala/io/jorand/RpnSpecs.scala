package io.jorand

import org.scalatest.{Matchers, WordSpecLike}
import cats.data.Validated.{invalid, valid}
import cats.data.NonEmptyList
import io.jorand.RPN._


/**
  * Sample specification.
  */
class RPNSpecs extends WordSpecLike with Matchers {
  "The RPN calculator" should {
    "perfom simple calculation" in {
      "1 2 +".calc should equal(valid(3))
    }

    "perfom complex calculation" in {
      "1 2 + 5 * 3 / 5 -".calc should equal(valid(0))
    }

    "return error if not a number in the header" in {
      "a 2 + 5 * 3 / 5 -".calc should equal(invalid(NonEmptyList.of(s"a ${RPN.NotNumber}")))
    }

    "return multiple errors if not a number in the header and wrong operator" in {
      "a 2 + 5 * 3 / 5 :".calc should equal(invalid(NonEmptyList.of(s"a ${RPN.NotNumber}",s": ${RPN.NotOperator}")))
    }

    "return multiple errors if not a number in the header, wrong operator and a bad number in the stack" in {
      "a 2 + 5 * 3 / b :".calc should equal(invalid(NonEmptyList.of(s"a ${RPN.NotNumber}",s": ${RPN.NotOperator}", s"b ${RPN.NotNumber}")))
    }


  }
}
