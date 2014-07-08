package samples

import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner._
import io.jorand.kata.RPN._
  

/**
 * Sample specification.
 * 
 * This specification can be executed with: scala -cp <your classpath=""> ${package}.SpecsTest
 * Or using maven: mvn test
 *
 * For more information on how to write or run specifications, please visit: 
 *   http://etorreborre.github.com/specs2/guide/org.specs2.guide.Runners.html
 *
 */
@RunWith(classOf[JUnitRunner])
class RPNSpecTest extends Specification {
  "The RPN Calculator" should {
    "accept a function as parameter" in {
      calcBin(List[BigDecimal](1,2,3), _ + _) === List[BigDecimal](1,5)
    }
    "consume correctly the stack" in {
      calcBin(List[BigDecimal](1,2,3), _ + _).size === 2
    }
    "perform simple calculation" in {
      "3 4 *".calc === 12
    }
    "perform simple negative calculation" in {
      "-3 4 *".calc === -12
    }
    "perform complex calculation" in {
      "2 3 + 5 / 10 *".calc === 10
    }
  }
}
