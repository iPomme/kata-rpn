package io.jorand.kata

import org.scalactic._
import Accumulation._

/**
 * @author Nicolas Jorand
 */
object RPN {
  val notValidOperator = "is not a valid operator"

  val notValidNumber = "is not a valid number"


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
        case e: Exception => Bad(One(s"'$num' $notValidNumber"))
      }
    }

    def isOperator(oper : String) = {
      if("+-*/".contains(oper))
        Good(oper)
      else
        Bad(One(s"'$oper' $notValidOperator"))
    }

    def check(stackList: List[String]) = {
      val header = stackList.take(2).map(isNumeric)
      val number = stackList.drop(3).sliding(1,2).flatten.map(isNumeric)
      val operant = stackList.drop(2).sliding(1,2).flatten.map(isOperator)

      List(header, number, operant).flatten.combined
    }

    def calc = {
      val stackParsed = parse()
      val checkedStack = check(stackParsed.get)

      withGood(stackParsed, checkedStack) { (s, checked) =>
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
