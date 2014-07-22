package io.jorand.kata

import org.scalactic._
import Accumulation._

/**
 * @author Nicolas Jorand
 */
object RPN {
  val BadOperant = "is a bad operant"

  val BadNumber = "is a bad number"


  implicit class RPNCalculator(stack: String) {

    def parse() = {
      try {
        Good(stack.split(" ").toList)
      } catch {
        case e: Exception => Bad(One(e.getMessage))
      }
    }

    def isNumeric(s:String) = {
      try{
        Good(BigDecimal(s))
      }catch {
        case e :Exception => Bad(One(s"'$s' $BadNumber"))
      }
    }
    def isOperant(s:String) = {
      if("+-*/".contains(s))
        Good(s)
      else
        Bad(One(s"'$s' $BadOperant"))
    }

    def checkStack(stack: List[String]) = {
      val checkHead = stack.take(2).map(isNumeric)
      val checkNumbers = stack.drop(3).sliding(1, 2).flatten.map(isNumeric)
      val checkOperant = stack.drop(2).sliding(1,2).flatten.map(isOperant)

      List(checkHead, checkNumbers, checkOperant).flatten.combined

    }

    def calc(): BigDecimal Or Every[ErrorMessage] = {
      val parsed = parse()
      val checking = checkStack(parsed.get)
      withGood(parsed, checking) { (s, c) =>
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
