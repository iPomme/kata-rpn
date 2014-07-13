package io.jorand.kata

import org.scalactic._
import Accumulation._

/**
 * @author Nicolas Jorand
 */
object RPN {

  implicit class RPNCalculator(s: String) {

    def parseString(): List[String] Or One[ErrorMessage] = {
      try {
        Good(s.split(" ").toList)
      } catch {
        case e: Exception => Bad(One(e.getMessage))
      }
    }

    def checkHead(stack: List[String]): Validation[ErrorMessage] = {
      try {
        BigDecimal(stack(0))
        BigDecimal(stack(1))
        Pass
      } catch {
        case e: Exception => Fail("The two first items of the queue should contains two numbers")
      }
    }

    def checkOperators(stack: List[String]): Validation[ErrorMessage] = {
      val operators = stack.drop(2).sliding(1, 2).flatten.filter(!"+-*/".contains(_)).toList
      if (operators.isEmpty) Pass else Fail(s"Invalid character '${operators.foldLeft("")(_ + _)}'")
    }

    def isNumeric(s: String): Boolean = {
      try {
        BigDecimal(s)
        true
      } catch {
        case _ => false
      }
    }

    def checkNumbers(stack: List[String]): Validation[ErrorMessage] = {
      val numbers = stack.drop(3).sliding(1, 2).flatten.filter(!isNumeric(_)).toList
      if (numbers.isEmpty) Pass else Fail(s"'${numbers.foldLeft("")(_ + _)}' => not number(s)")
    }

    def calc(): BigDecimal Or Every[ErrorMessage] = {
      val stack = parseString() when(checkHead, checkOperators, checkNumbers)

      withGood(stack) {
        _.foldLeft(List[BigDecimal]()) {
          case (x :: y :: xs, "+") => y + x :: xs // Notice the invertion of x and y
          case (x :: y :: xs, "-") => y - x :: xs // Notice the invertion of x and y
          case (x :: y :: xs, "*") => y * x :: xs // Notice the invertion of x and y
          case (x :: y :: xs, "/") => y / x :: xs // Notice the invertion of x and y
          case (list, num) => BigDecimal(num) :: list
        }.head
      }

    }
  }

}
