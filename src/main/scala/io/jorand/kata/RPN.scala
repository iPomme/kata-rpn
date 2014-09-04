package io.jorand.kata

import org.scalactic.Accumulation._
import org.scalactic._

/**
 * @author Nicolas Jorand
 */
object RPN {
  val BadOperator = "is a bad operator"

  val BadNumer = "is a bad number"


  implicit class RPNCalc(stack: String) {


    def parse() = {
      try {
        Good(stack.split(" ").toList)
      } catch {
        case e: Exception => Bad(One(e.getMessage))
      }
    }

    def isNumber(num: String) = {
      try {
        Good(BigDecimal(num))
      } catch {
        case e: Exception => Bad(One(s"'$num' $BadNumer"))
      }
    }

    def isOperator(oper: String) = {
      if ("+-*/".contains(oper))
        Good(oper)
      else
        Bad(One(s"'$oper' $BadOperator"))
    }

    def checkStack(s: List[String]) = {
      val header = s.take(2).map(isNumber)
      val numbers = s.drop(3).sliding(1, 2).flatten.map(isNumber)
      val operators = s.drop(2).sliding(1, 2).flatten.map(isOperator)

      List(header, numbers, operators).flatten.combined
    }

    def calc() = {
      val parsedStack = parse()
      val stackChecked = checkStack(parsedStack.get)

      withGood(parsedStack, stackChecked) { (s, c) =>

        s.foldLeft(List[BigDecimal]()) {
          case (x :: y :: xs, "+") => y + x :: xs // Notice the x and y reversed in the operation
          case (x :: y :: xs, "-") => y - x :: xs // Notice the x and y reversed in the operation
          case (x :: y :: xs, "*") => y * x :: xs // Notice the x and y reversed in the operation
          case (x :: y :: xs, "/") => y / x :: xs // Notice the x and y reversed in the operation
          case (list, num) => BigDecimal(num) :: list
        }.head
      }

    }
  }

}
