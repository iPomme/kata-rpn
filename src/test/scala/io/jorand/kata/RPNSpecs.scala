package io.jorand.kata

import io.jorand.kata.RPN._
import org.junit.runner.RunWith
import org.scalactic.{Many, One, Bad, Good}
import org.specs2.mutable._
import org.specs2.runner._
import io.jorand.kata.RPN._


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
      "1 -2 +".calc === Good(-1)
    }
    "perform complex calculation" in {
      "1 2 + 5 * 15 / 1 -".calc === Good(0)
    }

    "return numeric error" in {
      "1 a +".calc === Bad(One(s"'a' ${RPN.badNumber}"))
    }
    "return an operation error" in {
      "1 1 )".calc === Bad(One(s"')' ${RPN.badOperand}"))
    }
    "return mutliple errors" in {
      "1 1 ) d +".calc === Bad(Many( s"'d' ${RPN.badNumber}",s"')' ${RPN.badOperand}"))
    }

  }
}
