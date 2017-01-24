package io.jorand

import com.typesafe.scalalogging.Logger
import cats.data.Validated.{invalidNel, valid}
import cats.implicits._
import cats._

/**
  * @author Nicolas Jorand
  */
object RPN {

  val log = Logger[RPN.type]

  val NotNumber = "is no a number"
  val NotOperator = "is not a valid operator"

  implicit class RPNCalc(stack: String) {



    def isNumber(num: String) = {
      try {
        valid(BigDecimal(num))
      } catch {
        case e: Exception => invalidNel(s"$num $NotNumber")
      }
    }

    def isOperator(op: String) = {
      if ("+-*/".contains(op))
        valid(op)
      else
        invalidNel(s"$op $NotOperator")
    }

    def checkStack(s: List[String]) = {

      val header = s.take(2).map(isNumber).sequenceU_
      val operator = s.drop(2).sliding(1, 2).flatten.toList.map(isOperator).sequenceU_
      val number = s.drop(3).sliding(1, 2).flatten.toList.map(isNumber).sequenceU_

      header |@| operator |@| number
    }


    def calc() = {
      val parsedStack = stack.split(" ").toList

      checkStack(parsedStack).map { (_, _, _) =>
        parsedStack.foldLeft(List[BigDecimal]()) {
          case (x :: y :: xs, "+") => y + x :: xs
          case (x :: y :: xs, "-") => y - x :: xs
          case (x :: y :: xs, "/") => y / x :: xs
          case (x :: y :: xs, "*") => y * x :: xs
          case (list, num) => BigDecimal(num) :: list
        }.head
      }
    }
  }

}