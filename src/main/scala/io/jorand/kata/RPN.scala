package io.jorand.kata

import org.scalactic._
import org.scalactic.Accumulation._
import org.scalactic.One

/**
 * @author Nicolas Jorand
 */
object RPN {

  val BadOperand = "is a bad operand"
  val BadNumber = "is a bad number"


  implicit class RPNCalculator(stack: String) {

    def isNumeric(s: String): Or[BigDecimal, One[String]] = {
      try {
        Good(BigDecimal(s))
      } catch {
        case e: Exception => Bad(One(s"'$s' $BadNumber"))

      }
    }

    def isOperant(s: String): Or[String, One[String]] = {
      if ("+-*/".contains(s))
        Good(s)
      else
        Bad(One(s"'$s' $BadOperand"))
    }

    def check(sl: List[String]) = {
      val header = sl.take(2).map(isNumeric).combined
      val numbers = sl.drop(3).sliding(1, 2).flatten.map(isNumeric).combined
      val operands = sl.drop(2).sliding(1, 2).flatten.map(isOperant).combined

      List(header, numbers, operands).combined
    }

    def parse() = {
      try {
        Good(stack.split(" ").toList)
      } catch {
        case e: Exception => Bad(One(e.getMessage))
      }
    }

    def calc(): BigDecimal Or Every[ErrorMessage] = {
      val stackList = parse()
      val checking = check(stackList.get)

      withGood(stackList, checking) { (s, c) =>
        s.foldLeft(List[BigDecimal]()) {
          case (x :: y :: xs, "+") => y + x :: xs // Notice the x and y reversed.
          case (x :: y :: xs, "-") => y - x :: xs // Notice the x and y reversed.
          case (x :: y :: xs, "*") => y * x :: xs // Notice the x and y reversed.
          case (x :: y :: xs, "/") => y / x :: xs // Notice the x and y reversed.
          case (list, num) => BigDecimal(num) :: list
        }.head
      }
    }


  }

}
