package io.jorand.kata

import scala.util.Try

/**
 * @author ${user.name}
 */
object RPN {

  implicit class RPNCalculator(stack: String) {
    def calc(): Try[BigDecimal] = {
      Try {
        stack.split(" ").foldLeft(List[BigDecimal]()) {
          case (x :: y :: xs, "+") => y + x :: xs
          case (x :: y :: xs, "-") => y - x :: xs
          case (x :: y :: xs, "*") => y * x :: xs
          case (x :: y :: xs, "/") => y / x :: xs
          case (list, num) => BigDecimal(num) :: list
        }.head
      }
    }
  }

}
