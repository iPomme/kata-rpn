package io.jorand

import com.typesafe.scalalogging.Logger
import cats.data.Validated.{valid, invalidNel}
import cats.implicits._

/**
  * @author Nicolas Jorand
  */
object RPN {
  val NotOperator = "is not an operator"

  val NotNumber = "is not a number"


  val log = Logger[RPN.type]

  def isNumber(num: String) = {
    try {
      valid(BigDecimal(num))
    } catch {
      case e: Exception => invalidNel(s"$num $NotNumber")
    }
  }

  def isOperator(op :String)= {
    if("+-*/".contains(op))
      valid(op)
    else
      invalidNel(s"$op $NotOperator")
  }

  def checkStack(s: List[String]) = {
    val headers = s.take(2).map(isNumber).sequenceU_
    val operators = s.drop(2).sliding(1, 2).flatten.map(isOperator).toList.sequenceU_
    val numbers = s.drop(3).sliding(1, 2).flatten.map(isNumber).toList.sequenceU_
    headers |@| operators |@| numbers
  }

  implicit class RPMCalc(stack: String) {
    def calc() = {
      val parsedStack = stack.split(" ").toList

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