package io.jorand.kata

import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner._
import scala.util.Success
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
      "5 6 +".calc === Success(11)
    }
    "perform simple negative calculation" in {
      "5 -6 +".calc === Success(-1)
    }
    "perform complex calculation calculation" in {
      "5 6 + 11 / 5 * 2 -".calc === Success(3)
    }
    "perform simple calculation" in {
      "5 6 (".calc must not be Success(0)
    }

  }
}
