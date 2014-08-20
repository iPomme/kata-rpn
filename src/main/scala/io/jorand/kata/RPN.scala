package io.jorand.kata

import org.scalactic.Accumulation._
import org.scalactic._

/**
 * @author Nicolas Jorand
 */
object RPN {

  val notNumber = "not a valid number"
  val notOperant = "not a valid operant"

  implicit class RPNCalc(stack: String) {

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
        case e :Exception => Bad(One(s"'$num' $notNumber"))
      }
    }

    def isOperant(oper: String) = {
      if ("+_*/".contains(oper))
        Good(oper)
      else
        Bad(One(s"'$oper' $notOperant"))
    }

    def checkStack(s: List[String]) = {
      // Check the first two number
      val header = s.take(2).map(isNumeric)
      // check the operant
      val operant = s.drop(2).sliding(1, 2).flatten.map(isOperant)
      // check the numbers
      val numbers = s.drop(3).sliding(1, 2).flatten.map(isNumeric)

      List(header, operant, numbers).flatten.combined
    }

    def calc: BigDecimal Or Every[String] = {
      val parsedStack = parse()

      val stackCheck = checkStack(parsedStack.get)

      withGood(parsedStack, stackCheck) { (s, check) =>
        s.foldLeft(List[BigDecimal]()) {
          case (x :: y :: xs, "+") => y + x :: xs // Notice the x y reversed in the operation
          case (x :: y :: xs, "-") => y - x :: xs // Notice the x y reversed in the operation
          case (x :: y :: xs, "*") => y * x :: xs // Notice the x y reversed in the operation
          case (x :: y :: xs, "/") => y / x :: xs // Notice the x y reversed in the operation
          case (list, num) => BigDecimal(num) :: list
        }.head

      }

    }

  }

}
