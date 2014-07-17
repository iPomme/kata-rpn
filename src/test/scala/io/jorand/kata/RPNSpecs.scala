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
      "1 2 +".calc === Good(3)
    }
    "perform simple negative calculation" in {
      "1 -2 +".calc === Good(-1)
    }
    "perform complex calculation" in {
      "1 2 + 3 * 9 / 10 -".calc === Good(-9)
    }
    "return error with bad number for the head" in {
      "1 a + 3 * 9 / 10 -".calc === Bad(One(s"'a' ${RPN.BadNumber}"))
    }
    "perform complex calculation" in {
      "1 a + gg * 9 / 10 )".calc === Bad(Many(s"'a' ${RPN.BadNumber}", s"'gg' ${RPN.BadNumber}", s"')' ${RPN.BadOperant}"))
    }

  }
}
