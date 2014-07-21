package io.jorand.kata

import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner._
import org.scalactic.{Many, Bad, One, Good}
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
  "The calculator" should {
    "perform simple calculation" in {
      "2 3 +".calc === Good(5)
    }
    "perform simple negative calculation" in {
      "2 -3 +".calc === Good(-1)
    }
    "perform complex calculation" in {
      "2 3 + 5 * 25 / 1 -".calc === Good(0)
    }
    "receive one error when parsing error" in {
      "2 s +".calc === Bad(One(s"'s' ${RPN.BadNumber}"))
    }
    "receive multiple error when parsing error" in {
      "2 s + 4 ) s -".calc === Bad(Many(s"'s' ${RPN.BadNumber}",s"'s' ${RPN.BadNumber}",s"')' ${RPN.BadOperand}"))
    }

  }
}
