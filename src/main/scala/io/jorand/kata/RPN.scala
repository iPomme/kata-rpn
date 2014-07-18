package io.jorand.kata

import org.scalactic._
import org.scalactic.One
import Accumulation._

/**
 * @author Nicolas Jorand
 */
object RPN {
  val BADOperator = "is a bad operator"

  val BADNumber = "is a bad number"


  implicit class RPNCalculator(stack: String) {

    def parse(): List[String] Or One[ErrorMessage] = {
      try {
        Good(stack.split(" ").toList)
      } catch {
        case e: Exception => Bad(One(e.getMessage))
      }
    }

    def isNumeric(s: String): BigDecimal Or One[ErrorMessage] = {
      try {
        Good(BigDecimal(s))
      } catch {
        case e: Exception => Bad(One(s"'$s' $BADNumber"))
      }
    }

    def isOperant(s: String): String Or One[ErrorMessage] = {
      if ("+-*/".contains(s))
        Good(s)
      else
        Bad(One(s"'$s' $BADOperator"))
    }

    def check(list: List[String]) = {
      val headCheck = list.take(2).map(isNumeric).combined
      val checkNumber = list.drop(3).sliding(1,2).flatten.map(isNumeric).combined
      val checkOperant = list.drop(2).sliding(1,2).flatten.map(isOperant).combined
      List(headCheck, checkNumber, checkOperant).combined
    }

    def calc(): BigDecimal Or Every[ErrorMessage] = {
      val stackList = parse()
      val checking = check(stackList.get)

      withGood(stackList, checking) { (gStack, c) =>

        gStack.foldLeft(List[BigDecimal]()) {
          case (x :: y :: xs, "+") => y + x :: xs //Notice the reverse of x and y in the operation
          case (x :: y :: xs, "-") => y - x :: xs //Notice the reverse of x and y in the operation
          case (x :: y :: xs, "*") => y * x :: xs //Notice the reverse of x and y in the operation
          case (x :: y :: xs, "/") => y / x :: xs //Notice the reverse of x and y in the operation
          case (list, num) => BigDecimal(num) :: list
        }.head
      }
    }

  }

}
