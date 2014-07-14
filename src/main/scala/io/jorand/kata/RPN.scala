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

    def isNumeric(num :String) : BigDecimal Or One[ErrorMessage] = {
      try{
        Good(BigDecimal(num))
      }catch {
        case e :Exception =>  Bad(One(s"'${num}' => must be number"))
      }
    }

    def checkOperants(stack : String) : String Or One[ErrorMessage] = {
      if("+-*/".contains(stack))
        Good(stack)
      else
        Bad(One(s"'${stack}' => bad operant"))
    }

    def calc(): BigDecimal Or Every[ErrorMessage] = {
      val stack = Good(s.split(" ").toList)

      val headCheck = stack.get.take(2).map(isNumeric).combined
      val checkNumber = stack.get.drop(3).sliding(1,2).flatten.map(isNumeric).combined
      val checkOperant = stack.get.drop(2).sliding(1,2).flatten.map(checkOperants).combined
      //Combine all the validation
      val checkedList = List(headCheck, checkNumber, checkOperant).combined



      withGood(stack, checkedList) { (s,e) =>
        s.foldLeft(List[BigDecimal]()){
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
