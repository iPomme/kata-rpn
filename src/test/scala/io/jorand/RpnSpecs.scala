package io.jorand

import org.scalatest.{Matchers, WordSpecLike}
import RPN._
import cats.data.{NonEmptyList, OneAnd}
import cats.data.Validated.{Invalid, Valid}


/**
  * Sample specification.
  */
class RPNSpecs extends WordSpecLike with Matchers {
  "The RPN calculator" should {
    "compute simple operation" in {
      "1 2 +".calc should be(Valid(3))
    }

    "compute complicated operations" in {
      "1 2 + 3 * 9 / 1 -".calc should be(Valid(0))
    }

    "get invalid result when error on the header" in {
      "a 2 + 3 * 9 / 1 -".calc should be(Invalid(NonEmptyList.of(s"a ${RPN.NotNumber}")))
    }

    "get invalid result when error on the operator" in {
      "1 2 . 3 * 9 / 1 -".calc should be(Invalid(NonEmptyList.of(s". ${RPN.NotOperator}")))
    }

    "get invalid result when error on number" in {
      "1 2 + a * 9 / 1 -".calc should be(Invalid(NonEmptyList.of(s"a ${RPN.NotNumber}")))
    }

    "get multiple invalid result when  multiple error on the stack" in {
      "1 2 a a * 9 / 1 -".calc should be(Invalid(NonEmptyList.fromListUnsafe(List(s"a ${RPN.NotOperator}", s"a ${RPN.NotNumber}"))))
    }

  }
}
