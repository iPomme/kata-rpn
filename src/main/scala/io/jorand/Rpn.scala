package io.jorand

import com.typesafe.scalalogging.Logger
import cats.data.Validated.{valid, invalidNel}
import cats.implicits._

/**
  * @author Nicolas Jorand
  */
object RPN {
  val NotValidOperator = "not a valid operator"

  val NotValidNumber = "is not a valid number"


  val log = Logger[RPN.type]

  implicit class RPNCalculator(stack: String) {

    def isNumber(n: String) = {
      try {
        valid(BigDecimal(n))
      } catch {
        case e: Exception => invalidNel(s"$n $NotValidNumber")
      }
    }

    def isOperator(op: String) = {
      if ("+-*/".contains(op))
        valid(op)
      else invalidNel(s"$op $NotValidOperator")
    }

    def checkStack(parsedStack: List[String]) = {
      val header = parsedStack.take(2).map(isNumber).sequenceU_
      val operator = parsedStack.drop(2).sliding(1, 2).flatten.map(isOperator).toList.sequenceU_
      val numbers = parsedStack.drop(3).sliding(1, 2).flatten.map(isNumber).toList.sequenceU_

      header |@| operator |@| numbers
    }

    def calc() = {
      val parsedStack = stack.split(" ").toList
      val ckeckedStack = checkStack(parsedStack)

      ckeckedStack.map { (_, _, _) =>

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