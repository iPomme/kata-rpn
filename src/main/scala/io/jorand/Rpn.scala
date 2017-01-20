package io.jorand

import com.typesafe.scalalogging.Logger
import cats.data.Validated.{invalidNel, valid}
import cats._
import cats.data.{NonEmptyList, Validated}
import cats.implicits._


/**
  * @author Nicolas Jorand
  */
object RPN {
  val NOTOPERATOR = "is not an operator"

  val NOTNUMBER = "is not a number"


  val log = Logger[RPN.type]

  implicit class RPNCalc(stack: String) {

    def isNumber(num: String): Validated[NonEmptyList[String], BigDecimal] = {
      try {
        valid(BigDecimal(num))
      } catch {
        case e: Exception => invalidNel(s"$num $NOTNUMBER")
      }
    }

    def isOperator(op: String): Validated[NonEmptyList[String], String] = {
      if ("+_*/".contains(op))
        valid(op)
      else
        invalidNel(s"$op $NOTOPERATOR")
    }

    def checkStack(s: List[String]) = {
      val headerCheck = s.take(2).map(isNumber).sequenceU_
      val operatorCheck = s.drop(2).sliding(1, 2).flatten.map(isOperator).toList.sequenceU_
      val numberCheck = s.drop(3).sliding(1, 2).flatten.map(isNumber).toList.sequenceU_

      headerCheck |@| operatorCheck |@| numberCheck
    }

    def parse() = {
      stack.split(" ").toList
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