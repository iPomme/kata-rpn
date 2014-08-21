package io.jorand.kata

import org.scalactic.Accumulation._
import org.scalactic._

/**
 * @author Nicolas Jorand
 */
object RPN {

  val badNumber = "not a valid number"
  val badOperand = "not a valid operant"

  implicit class RPNCalc(stack: String) {

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

    def isOperant(oper: String) = {
      if ("+-*/".contains(oper))
        Good(oper)
      else
        Bad(One(s"'$oper' $badOperand"))
    }

    def checkStack(st: List[String]) = {
      val header = st.take(2).map(isNumeric)
      val numbers = st.drop(3).sliding(1, 2).flatten.map(isNumeric)
      val operands = st.drop(2).sliding(1, 2).flatten.map(isOperant)

      // combine the check together
      List(header, numbers, operands).flatten.combined
    }

    def calc = {
      val stackParsed = parse()

      val checkedStack = checkStack(stackParsed.get)

      withGood(stackParsed, checkedStack) { (s, cStack) =>
        s.foldLeft(List[BigDecimal]()) {
          case (x :: y :: xs, "+") => y + x :: xs // notice the reverse of x and y in the operation
          case (x :: y :: xs, "-") => y - x :: xs // notice the reverse of x and y in the operation
          case (x :: y :: xs, "*") => y * x :: xs // notice the reverse of x and y in the operation
          case (x :: y :: xs, "/") => y / x :: xs // notice the reverse of x and y in the operation
          case (list, num) => BigDecimal(num) :: list
        }.head
      }
    }
  }

}
