package io.jorand

import com.typesafe.scalalogging.Logger
import cats.data.Validated.{invalidNel, valid}
import cats.implicits._


/**
  * @author Nicolas Jorand
  */
object RPN {

  val NotNumber = "is not a number"
  val NotOperator = "is not an operator"


  implicit class RPNCalc(stack: String) {


    def parse() = {
      stack.split(" ").toList
    }

    def isNumber(num : String) = {
      try{
        valid(BigDecimal(num))
      }catch {
        case e : Exception => invalidNel(s"$num $NotNumber")
      }
    }

    def isOperator(op: String) = {
      if("+-*/".contains(op))
        valid(op)
      else
        invalidNel(s"$op $NotOperator")
    }

    def checkStack(s : List[String]) = {
      val checkedHeader = s.take(2).map(isNumber).sequenceU_
      val checkOperator = s.drop(2).sliding(1,2).flatten.toList.map(isOperator).sequenceU_
      val checkNumer = s.drop(3).sliding(1,2).flatten.toList.map(isNumber).sequenceU_

      checkedHeader |@| checkOperator |@| checkNumer
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