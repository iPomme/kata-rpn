package io.jorand.kata

import scala.util.Try

/**
 * @author Nicolas Jorand
 */
object RPN {
  
  implicit class RPNCalculator(s :String) {

    def calc() : Try[BigDecimal] = {
      Try {
        s.split(" ").foldLeft(List[BigDecimal]()) {
          case (x :: y :: xs, "+") => y + x :: xs // Notice the invertion of x and y
          case (x :: y :: xs, "-") => y - x :: xs // Notice the invertion of x and y
          case (x :: y :: xs, "*") => y * x :: xs // Notice the invertion of x and y
          case (x :: y :: xs, "/") => y / x :: xs // Notice the invertion of x and y
          case(list, num) => BigDecimal(num) :: list
        }.head
      }
    }
  }
}
