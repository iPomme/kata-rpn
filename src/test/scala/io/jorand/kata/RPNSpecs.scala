package io.jorand.kata

import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner._
import org.scalactic.{Many, One, Bad, Good}
import io.jorand.kata.RPN._


/**
 * Sample specification.
 * 
 * This specification can be executed with: scala -cp <your classpath=""> io.jorand.kata.SpecsTest
 * Or using maven: mvn test
 *
 * For more information on how to write or run specifications, please visit: 
 *   http://etorreborre.github.com/specs2/guide/org.specs2.guide.Runners.html
 *
 */
@RunWith(classOf[JUnitRunner])
class RPNSpecTest extends Specification {
  "The RPN Calculator" should {
    "perform simple calculation" in {
      "1 3 +".calc === Good(4)
    }
    "perform simple negative calculation" in {
      "1 5 -".calc === Good(-4)
    }
    "perform complex calculation" in {
      "1 3 + 4 * 16 / 1 -".calc === Good(0)
    }
    "return one error" in {
      "1 a + 4 * 16 / 1 -".calc === Bad(One(s"'a' ${RPN.BADNumber}"))
    }
    "return multiple errors" in {
      "1 a + zz * 16 ] 1 -".calc === Bad(Many(s"'a' ${RPN.BADNumber}",s"'zz' ${RPN.BADNumber}",s"']' ${RPN.BADOperator}"))
    }

  }
}
