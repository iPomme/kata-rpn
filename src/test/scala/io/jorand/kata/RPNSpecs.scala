package io.jorand.kata

import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner._
import io.jorand.kata.RPN._
import org.scalactic.{Many, One, Bad, Good}


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
  "The RPN Calculator" should {
    "perform simple calculation" in {
      "5 6 +".calc === Good(11)
    }
    "perform simple negative calculation" in {
      "5 -6 +".calc === Good(-1)
    }
    "perform complex calculation calculation" in {
      "5 6 + 11 / 5 * 2 -".calc === Good(3)
    }
    "return error message with a bad operator" in {
      "5 6 (".calc === Bad(One("Invalid character '('"))
    }
    "return error messages with multiple errors" in {
      "5 6 ( a +".calc === Bad(Many("Invalid character '('","'a' => not number(s)"))
    }
    "return an error if the first two items are not a number" in {
      "a 6 +".calc === Bad(One("The two first items of the queue should contains two numbers"))
    }

  }
}
