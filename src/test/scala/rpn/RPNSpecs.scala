package io.jorand.kata

import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner._
import ReversePolishNotation._


/**
 * Specification to test the RPN engine
 * @author nicolas.jorand
 */
@RunWith(classOf[JUnitRunner])
class RPNSpecTest extends Specification {

  "The RPN calculator" should {
    "consume the stack with operand" in {
      calcSign(List[BigDecimal](1, 2, 3), (_ + _)) === List[BigDecimal](1, 5)
    }

  }
  "perform simple calculus" in {
    "3 2 +".calc === 5
  }
  "perform negative calculus" in {
    "-3 2 +".calc === -1
  }
  "perform decimal calculus" in {
    "-3.4 2 -".calc === BigDecimal(-5.4)
  }
  "perform calculus on complex stack" in {
    "4 2 - 6 + 8 -".calc === 0
    "1 2 + 4 * 5 + 3 -".calc === 14
  }
}
