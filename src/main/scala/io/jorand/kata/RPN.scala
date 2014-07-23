package io.jorand.kata

import org.scalactic._
import org.scalactic.Bad
import Accumulation._

/**
 * @author Nicolas Jorand
 */
object RPN {
  val BadOperator = "is a bad operator"

  val BadNumer = "is a bad number"

  implicit class RPNCalculator(stack: String) {


    def parse(): List[String] Or One[String] = {
      try {
        Good(stack.split(" ").toList)
      } catch {
        case e: Exception => Bad(One(e.getMessage))
      }
    }

    def isNumeric(num: String) = {
      try {
        Good(BigDecimal(num))
      } catch {
        case e: Exception => Bad(One(s"'$num' $BadNumer"))
      }
    }

    def isOperant(oper: String) = {
      if ("+-*/".contains(oper))
        Good(oper)
      else
        Bad(One(s"'$oper' $BadOperator"))
    }

    def checkStack(s: List[String]) = {
      val headers = s.take(2).map(isNumeric)
      val numbers = s.drop(3).sliding(1, 2).flatten.map(isNumeric)
      val operant = s.drop(2).sliding(1, 2).flatten.map(isOperant)

      List(headers, numbers, operant).flatten.combined
    }

    def calc(): BigDecimal Or Every[ErrorMessage] = {
      val parsedStack = parse()
      val stackCheck = checkStack(parsedStack.get)

      withGood(parsedStack, stackCheck) { (s, c) =>
        s.foldLeft(List[BigDecimal]()) {
          case (x :: y :: xs, "+") => y + x :: xs // Notice the x and y reversed in the operation !
          case (x :: y :: xs, "-") => y - x :: xs // Notice the x and y reversed in the operation !
          case (x :: y :: xs, "*") => y * x :: xs // Notice the x and y reversed in the operation !
          case (x :: y :: xs, "/") => y / x :: xs // Notice the x and y reversed in the operation !
          case (list, num) => BigDecimal(num) :: list
        }.head
      }
    }
  }

}
