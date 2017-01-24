package io.jorand

import org.scalatest.{Matchers, WordSpecLike}
import cats.data.Validated.{valid, invalid, invalidNel}
import cats.data.NonEmptyList
import RPN._

/**
  * Sample specification.
  */
class RPNSpecs extends WordSpecLike with Matchers {
  "The calculator" should {
    "perform simple calculation" in {
      "1 2 +".calc should equal(valid(3))
    }

    "perform complex calculation" in {
      "1 2 + 5 * 3 / 5 -".calc should equal(valid(0))
    }

    "return an error if wrong header" in {
      "a 2 + 5 * 3 / 5 -".calc should equal(invalidNela
      (s"a ${RPN.NotNumber}"))
    }

    "return multiple errors if wrong header, operator and numbers" in {
      "a 2 + 5 ? 3 / bb -".calc should equal(invalid(NonEmptyList.of(s"a ${RPN.NotNumber}", s"? ${RPN.NotOperator}", s"bb ${RPN.NotNumber}")))
    }

  }
}
