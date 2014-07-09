package io.jorand.kata

import scala.util.Try

/**
 * @author ${user.name}
 */
object RPN {

  implicit class RPNCalculator(s: String) {
    def calc(): Try[BigDecimal] = {
      Try {
        s.split(" ").foldLeft(List[BigDecimal]()) {
          case (x :: y :: xs, "+") => y + x :: xs
          case (x :: y :: xs, "-") => y - x :: xs
          case (x :: y :: xs, "*") => y * x :: xs
          case (x :: y :: xs, "/") => y / x :: xs
          case (xs, x) => BigDecimal(x) :: xs
        }.head
      }
    }
  }

}
