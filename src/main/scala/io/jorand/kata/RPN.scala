package io.jorand.kata

/**
 * @author ${user.name}
 */
object RPN {

  implicit class Calculator(s : String)  {
    def calc() : BigDecimal  = {
      s.split(" ").foldLeft(List[BigDecimal]())((out, in) =>
      in match {
        case "+" => calcBin(out, _ + _)
        case "-" => calcBin(out, _ - _)
        case "*" => calcBin(out, _ * _)
        case "/" => calcBin(out, _ / _)
        case _ => out :+ BigDecimal(in)
      }
      ).head
    }
  }

  def calcBin(numbers: List[BigDecimal],f: (BigDecimal, BigDecimal) => BigDecimal) : List[BigDecimal] = {
    numbers.dropRight(2) :+ f(numbers.dropRight(1).last, numbers.last)
  }

}
