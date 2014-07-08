package io.jorand.kata

/**
 * @author Nicolas Jorand
 */
object RPN {

  /**
   * Perform the calculation on a  RPN stack.
   * @param s the stack to perform calculation.
   */
  implicit class Calculator(s: String) {
    def calc(): BigDecimal = {
      s.split(" ").foldLeft(List[BigDecimal]())((out, in) => {
        in match {
          case "+" => calcBin(out, _ + _)
          case "-" => calcBin(out, _ - _)
          case "*" => calcBin(out, _ * _)
          case "/" => calcBin(out, _ / _)
          case _ => out :+ BigDecimal(in)
        }
      }).head
    }
  }

  /**
   * Apply the function to the two last arguments and return the new stack with the result at the end of the list
   * @param stack The current stack
   * @param f function to apply to the last two of the list
   * @return a new stack with the result of the operation on last position
   */
  def calcBin(stack: List[BigDecimal], f: (BigDecimal, BigDecimal) => BigDecimal): List[BigDecimal] = {
    stack.dropRight(2) :+ f(stack.dropRight(1).last, stack.last)
  }

}
