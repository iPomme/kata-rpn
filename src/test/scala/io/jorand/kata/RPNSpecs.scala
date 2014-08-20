package io.jorand.kata

import io.jorand.kata.RPN._
import org.junit.runner.RunWith
import org.scalactic._
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
      "1 -2 + 5 + 4 * 16 /".calc === Good(1)
    }


    "catch number exception" in {
      "a -2 +".calc === Bad(One(s"'a' ${RPN.notNumber}"))
    }

    "catch operation exception" in {
      "2 -2 =".calc === Bad(One(s"'=' ${RPN.notOperant}"))
    }

    "catch multiple errors" in {
      "2 -2 = a )".calc === Bad(Many(s"'=' ${RPN.notOperant}", s"')' ${RPN.notOperant}", s"'a' ${RPN.notNumber}"))
    }


  }
}
