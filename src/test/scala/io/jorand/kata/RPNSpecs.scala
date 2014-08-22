package io.jorand.kata

import io.jorand.kata.RPN._
import org.junit.runner.RunWith
import org.scalactic.{Bad, Good, Many, One}
import org.specs2.mutable._
import org.specs2.runner._


/**
 * Sample specification.
 *
 * This specification can be executed with: scala -cp <your classpath=""> io.jorand.kata.SpecsTest
 * Or using maven: mvn test
 *
 * For more information on how to write or run specifications, please visit: 
 * http://etorreborre.github.com/specs2/guide/org.specs2.guide.Runners.html
 *
 */
@RunWith(classOf[JUnitRunner])
class RPNSpecTest extends Specification {
  "The RPN calculator" should {
    "perform simple calculation" in {
      "1 2 +".calc === Good(3)
    }
    "perform simple negative calculation" in {
      "1 -2 +".calc === Good(-1)
    }

    "perform complex calculation" in {
      "1 2 + 3 * 9 / 1 -".calc === Good(0)
    }

    "return error on first two number" in {
      "1 a + 3 * 9 / 1 -".calc === Bad(One(s"'a' ${RPN.notValidNumber}"))
    }
    "return error on bad operator" in {
      "1 2 ? 3 * 9 / 1 -".calc === Bad(One(s"'?' ${RPN.notValidOperator}"))
    }
    "return multiple errors " in {
      "1 a ? 3 * 9 / 1 )".calc === Bad(Many(s"'a' ${RPN.notValidNumber}", s"'?' ${RPN.notValidOperator}", s"')' ${RPN.notValidOperator}"))
    }

  }
}
