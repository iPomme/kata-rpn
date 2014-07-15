package io.jorand.kata

import org.scalactic._
import Accumulation._

/**
 * @author Nicolas Jorand
 */
object RPN {

  implicit class RPNCalculator(stack: String) {

    def parseStack(): List[String] Or One[ErrorMessage] = {
      try {
        Good(stack.split(" ").toList)
      } catch {
        case e: Exception => Bad(One(e.getMessage))
      }
    }

    private[kata] def isNumeric(s: String): BigDecimal Or One[ErrorMessage] = {
      try {
        Good(BigDecimal(s))
      } catch {
        case e: Exception => Bad(One(s"'$s' => not a number"))
      }
    }

    private[kata] def isOperant(s: String): String Or One[ErrorMessage] = {
      if ("+-*/".contains(s))
        Good(s)
      else
        Bad(One(s"'$s' => not a valid operator"))
    }

    def calc(): BigDecimal Or Every[ErrorMessage] = {
      val parsedStack = parseStack()

      val checkHead = parsedStack.get.take(2).map(isNumeric).combined
      val checkNumbers = parseStack().get.drop(3).sliding(1, 2).flatten.map(isNumeric).combined
      val checkOperants = parseStack().get.drop(2).sliding(1, 2).flatten.map(isOperant).combined

      val allChecking = List(checkHead, checkNumbers, checkOperants).combined

      withGood(parsedStack, allChecking) { (s, c) =>
        s.foldLeft(List[BigDecimal]()) {
          case (x :: y :: xs, "+") => y + x :: xs // Notice x and y are reversed in the operation
          case (x :: y :: xs, "-") => y - x :: xs // Notice x and y are reversed in the operation
          case (x :: y :: xs, "*") => y * x :: xs // Notice x and y are reversed in the operation
          case (x :: y :: xs, "/") => y / x :: xs // Notice x and y are reversed in the operation
          case (list, num) => BigDecimal(num) :: list
        }.head
      }

    }
  }

}
