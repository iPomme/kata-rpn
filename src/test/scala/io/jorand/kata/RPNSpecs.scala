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
  "The RPN Calculator" should {
    "perform simple calculation" in {
      "1 2 +".calc === Good(3)
    }
    "perform simple negative calculation" in {
      "1 -2 +".calc === Good(-1)
    }
    "perform complex calculation" in {
      "1 2 + 3 * 9 / 1 -".calc === Good(0)
    }
    "validate the stack with an error on the first number" in {
      "a 2 + 3 * 9 / 1 -".calc === Bad(One("'a' => not a number"))
    }
    "validate the stack with multiple errors" in {
      "a 2 & 3 * z / 1 [".calc === Bad(Many("'a' => not a number", "'z' => not a number", "'&' => not a valid operator", "'[' => not a valid operator"))
    }

  }
}
