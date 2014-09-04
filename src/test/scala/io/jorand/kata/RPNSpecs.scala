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
  "The Calculator" should {
    "perform simple calculation" in {
      "1 2 +".calc === Good(3)
    }
    "perform simple negative calculation" in {
      "1 2 -".calc === Good(-1)
    }
    "perform complex calculation" in {
      "1 2 + 3 * 9 / 1 -".calc === Good(0)
    }

    "return an error in case of bad header" in {
      "a 1 +".calc === Bad(One(s"'a' ${RPN.BadNumer}"))
    }
    "return an error in case of bad operator" in {
      "1 1 )".calc === Bad(One(s"')' ${RPN.BadOperator}"))
    }
    "return multiple errors" in {
      "1 1 ) s +".calc === Bad(Many(s"'s' ${RPN.BadNumer}", s"')' ${RPN.BadOperator}"))
    }
  }
}
