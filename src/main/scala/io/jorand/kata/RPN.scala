package io.jorand.kata

import org.scalactic._
import org.scalactic.One
import Accumulation._

/**
 * @author Nicolas Jorand
 */
object RPN {
  val BadOperant = "is not a valid operant"

  val BadNumber = "is a bad number"


  implicit class RPNCalculator(s: String) {

    def parse(): List[String] Or One[ErrorMessage] = {
      try {
        Good(s.split(" ").toList)
      } catch {
        case e: Exception => Bad(One(e.getMessage))
      }
    }

    def isNumber(num: String): BigDecimal Or One[ErrorMessage] = {
      try {
        Good(BigDecimal(num))
      } catch {
        case e: Exception => Bad(One(s"'$num' $BadNumber"))
      }
    }

    def isOperant(opr: String): String Or One[ErrorMessage] = {
      if ("+-*/".contains(opr))
        Good(opr)
      else
        Bad(One(s"'$opr' $BadOperant"))
    }

    def calc(): BigDecimal Or Every[ErrorMessage] = {
      val stack = parse()
      val checkHeader = stack.get.take(2).map(isNumber).combined
      val checkNumber = stack.get.drop(3).sliding(1, 2).flatten.map(isNumber).combined
      val checkOperant = stack.get.drop(2).sliding(1, 2).flatten.map(isOperant).combined

      val allChecking = List(checkHeader, checkNumber, checkOperant).combined

      withGood(stack, allChecking) { (s , c) =>
        s.foldLeft(List[BigDecimal]()){
          case (x :: y :: xs, "+") => y + x :: xs // Notice the variable x and y are reversed in the calculation
          case (x :: y :: xs, "-") => y - x :: xs // Notice the variable x and y are reversed in the calculation
          case (x :: y :: xs, "*") => y * x :: xs // Notice the variable x and y are reversed in the calculation
          case (x :: y :: xs, "/") => y / x :: xs // Notice the variable x and y are reversed in the calculation
          case (list, num) => BigDecimal(num) :: list
        }.head
      }
    }


  }

}
