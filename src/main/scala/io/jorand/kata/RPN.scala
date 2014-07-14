package io.jorand.kata

import org.scalactic._
import org.scalactic.Many
import org.scalactic.One
import Accumulation._

/**
 * @author Nicolas Jorand
 */
object RPN {

  implicit class RPNCalculator(s: String) {

    def isNumeric(num :String) : Boolean = {
      try{
        BigDecimal(num)
        true
      }catch {
        case e :Exception =>  false
      }
    }

    def parse(): List[String] Or One[ErrorMessage] = {
      try {
        Good(s.split(" ").toList)
      } catch {
        case e: Exception => Bad(One(e.getMessage))
      }
    }

    def checkHead(stack : List[String]) : Validation[ErrorMessage] = {
      if(isNumeric(stack(0)) && isNumeric(stack(1)))
        Pass
      else
        Fail("The first operands need to be numeric")
    }

    def checkNumbers(stack : List[String]) : Validation[ErrorMessage] = {
      val badNumbers = stack.drop(3).sliding(1,2).flatten.filter(!isNumeric(_))
      if(badNumbers isEmpty)
        Pass
      else
        Fail(s"'${badNumbers.foldLeft("")(_ + _)}' => must be number")
    }

    def checkOperants(stack : List[String]) : Validation[ErrorMessage] = {
      val badOperants = stack.drop(2).sliding(1,2).flatten.filter(!"+-*/".contains(_))
      if(badOperants isEmpty)
        Pass
      else
        Fail(s"'${badOperants.foldLeft("")(_ + _)}' => bad operant")
    }

    def calc(): BigDecimal Or Every[ErrorMessage] = {
      val stack = parse().when(checkHead, checkNumbers, checkOperants)

      withGood(stack) {
        _.foldLeft(List[BigDecimal]()){
          case (x :: y :: xs, "+") => y + x :: xs // Notice x and y are reverse in the operation
          case (x :: y :: xs, "-") => y - x :: xs // Notice x and y are reverse in the operation
          case (x :: y :: xs, "*") => y * x :: xs // Notice x and y are reverse in the operation
          case (x :: y :: xs, "/") => y / x :: xs // Notice x and y are reverse in the operation
          case (list, num) => BigDecimal(num) :: list
        }.head
      }
    }
  }

}
