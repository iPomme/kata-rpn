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
 * http://etorreborre.github.com/specs2/guide/org.specs2.guide.Runners.html
 *
 */
@RunWith(classOf[JUnitRunner])
class RPNSpecTest extends Specification {
  "The Reverse Polish Notation Calculator" should {
    "perform simple operation" in {
      "1 2 +".calc === 3
    }
    "perform simple negative operation'" in {
      "-2 1 +".calc === -1
    }
    "perform complex calculation" in {
      "1 2 + 3 * 9 /".calc === 1
    }

    "should accept function as parameters" in {
      calcBin(List[BigDecimal](4, 5, 3), _ + _).size === 2
    }

    "should accept function as parameters" in {
      calcBin(List[BigDecimal](4, 5, 3), _ + _) === List[BigDecimal](4, 8)
    }
  }
}
