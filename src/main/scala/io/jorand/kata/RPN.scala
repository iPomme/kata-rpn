package io.jorand.kata

/**
 * @author ${user.name}
 */
object RPN {
  
  implicit class RPNCalculator(s: String) {

    def calc() : BigDecimal = {
      s.split(" ").foldLeft(List[BigDecimal]()) {
        case (x :: y :: xs, "+") => println(s"$x + $y :: $xs"); xs :+ x + y
        case (x :: y :: xs, "-") => println(s"$x - $y :: $xs"); xs :+ x - y
        case (x :: y :: xs, "*") => println(s"$x * $y :: $xs"); xs :+ x * y
        case (x :: y :: xs, "/") => println(s"$x / $y :: $xs"); xs :+ x / y
        case (list, num) => println(s"$list $num"); list :+ BigDecimal(num)
      } head
    }

  }

}
