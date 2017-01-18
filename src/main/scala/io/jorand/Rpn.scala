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

    def isNumber(n: String) = {
      try {
        valid(BigDecimal(n))
      } catch {
        case e: Exception => invalidNel(s"$n $NotNumber")
      }
    }

    def isOperator(o: String) = {
      if ("+-*/".contains(o))
        valid(o)
      else
        invalidNel(s"$o $NotOperator")

    }

    def checkStack(parsedStack: List[String]) = {
      val checkHeader = parsedStack.take(2).map(isNumber).sequenceU_
      val checkOperator = parsedStack.drop(2).sliding(1, 2).flatten.toList.map(isOperator).sequenceU_
      val checkNumbers = parsedStack.drop(3).sliding(1, 2).flatten.toList.map(isNumber).sequenceU_
      checkHeader |@| checkOperator |@| checkNumbers
    }

    def calc() = {
      val parsedStack = parse()

      checkStack(parsedStack).map { (_, _, _) =>
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