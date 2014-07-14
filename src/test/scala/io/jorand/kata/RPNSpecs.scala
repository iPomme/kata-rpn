package io.jorand.kata

import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner._
import org.scalactic._
import io.jorand.kata.RPN._
import org.scalactic.Many
import org.scalactic.One


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
    "perfom simple operation" in {
      "2 3 +".calc === Good(5)
    }
    "perfom simple negative operation" in {
      "2 -3 +".calc === Good(-1)
    }
    "perfom complex operation" in {
      "2 3 + 5 * 10 / 0.5 -".calc === Good(2)
    }
    "validate the stack by checking the two first element" in {
      "2 a +".calc === Bad(One("'a' => must be number"))
    }
    "validate the stack by checking the operators and numbers" in {
      "2 a + 8 > b -".calc === Bad(Many("'a' => must be number", "'b' => must be number", "'>' => bad operant"))
    }
  }
}
