package io.jorand

import cats._
import cats.implicits._
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.data.Validated.{Invalid, Valid, invalidNel, valid}

/**
  * @author Nicolas Jorand
  */
object RPN {

  val NotNumber = "is not a number"
  val NotOperator = "is not an operator"

  implicit class RPNEngine(stack: String) {
    def parse() = {
      stack.split(" ").toList
    }

    def isNumber(n: String): ValidatedNel[String, BigDecimal] = {
      try {
        valid[NonEmptyList[String], BigDecimal](BigDecimal(n))
      } catch {
        case e: Exception => invalidNel(s"$n $NotNumber")
      }
    }

    def isOperator(o: String): ValidatedNel[String, String] = {
      if ("+-*/".contains(o))
        valid[NonEmptyList[String], String](o)
      else
        invalidNel(s"$o $NotOperator")

    }

    def checkHeader(parsedStack: List[String]) = {
      parsedStack.take(2).map(isNumber).sequenceU_
    }

    def checkOperator(parsedStack: List[String]) = {
      parsedStack.drop(2).sliding(1, 2).flatten.toList.map(isOperator).sequenceU_
    }

    def checkNumbers(parsedStack: List[String]) = {
      parsedStack.drop(3).sliding(1, 2).flatten.toList.map(isNumber).sequenceU_
    }

    def calc() = {
      val parsedStack = parse()

      val checkedStack = checkHeader(parsedStack) |@| checkOperator(parsedStack) |@| checkNumbers(parsedStack)

      checkedStack.map { (_, _, _) =>
        parsedStack.foldLeft(List[BigDecimal]()) {
          case (x :: y :: xs, "+") => y + x :: xs
          case (x :: y :: xs, "-") => y - x :: xs
          case (x :: y :: xs, "*") => y * x :: xs
          case (x :: y :: xs, "/") => y / x :: xs
          case (list, num) => BigDecimal(num) :: list
        }.head
      }

    }
  }

}