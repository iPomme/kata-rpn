package io.jorand.kata

import org.scalactic.Accumulation._
import org.scalactic.{Accumulation, Bad, Good, One}

/**
 * @author Nicolas Jorand
 */
object RPN {
  val badOperator = "is a bad operator"

  val badNumber = "is a bad number"


  implicit class RPNCalculator(stack: String) {


    def parse() = {
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
        case e: Exception => Bad(One(s"'$num' $badNumber"))
      }
    }

    def isOperator(oper: String) = {
      if ("+-*/".contains(oper))
        Good(oper)
      else
        Bad(One(s"'$oper' $badOperator"))
    }

    def check(s: List[String]) = {
      val head = s.take(2).map(isNumeric)
      val numbers = s.drop(3).sliding(1, 2).flatten.map(isNumeric)
      val operators = s.drop(2).sliding(1, 2).flatten.map(isOperator)

      List(head, numbers, operators).flatten.combined

    }

    def calc() = {
      val parsedStack = parse()
      val checkedStack = check(parsedStack.get)
      withGood(parsedStack, checkedStack) { (s,c) =>
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
