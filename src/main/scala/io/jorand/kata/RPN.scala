package io.jorand.kata

object ReversePolishNotation {

  implicit class RPN(input: String) {
    def calc: BigDecimal = {
      val stack = input.split(" ")

      stack.foldLeft(List[BigDecimal]())((out, in) => in match {
        case "+" => calcSign(out, _ + _)
        case "-" => calcSign(out, _ - _)
        case "*" => calcSign(out, _ * _)
        case "/" => calcSign(out, _ / _)
        case _ => out :+ BigDecimal(in)
      }).head
    }


  }

  private[kata] def calcSign(numbers: List[BigDecimal], operation: (BigDecimal, BigDecimal) => BigDecimal): List[BigDecimal] = {
    val stack = numbers.reverse
    val ret = (operation(stack.drop(1).head, stack.head) :: stack.drop(2)).reverse
    ret
  }

}