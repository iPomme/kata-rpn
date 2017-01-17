package io.jorand

import com.typesafe.scalalogging.Logger
import org.scalactic.{Bad, Good, One}
import org.scalactic.Accumulation._

/**
  * @author Nicolas Jorand
  */
object RPN {

  val log = Logger[RPN.type]

  val NotNumber = "is not a number."

  val NotOperand = "is not an openrand"

  implicit class RPNCalc(stack: String) {

    def parse() = {
      try {
        Good(stack.split(" ").toList)
      } catch {
        case e: Exception => Bad(One(e.getMessage))
      }
    }

    def isNumeric(n: String) = {
      try {
        Good(BigDecimal(n))
      } catch {
        case e: Exception => Bad(One(s"$n $NotNumber"))
      }
    }

    def isOperand(o: String) = {
      if ("+-*/".contains(o))
        Good(o)
      else
        Bad(One(s"$o $NotOperand"))
    }

    def checkStack(l: List[String]) = {
      val header = l.take(2).map(isNumeric)
      val operand = l.drop(2).sliding(1, 2).flatten.map(isOperand)
      val number = l.drop(3).sliding(1,2).flatten.map(isNumeric)

      List(header, number, operand ).flatten.combined
    }

    def calc() = {
      val parsedStack = parse()
      val stackChecked = checkStack(parsedStack.get)

      withGood(parsedStack, stackChecked) { (s, c) =>
        s.foldLeft(List[BigDecimal]()) {
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