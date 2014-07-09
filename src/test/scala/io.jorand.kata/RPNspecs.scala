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
  "The RPN calculator" should {
    "perform simple operation" in {
      "2 3 +".calc === 5
    }
    "perform simple negative operation" in {
      "-2 3 -".calc === -5
    }
    "perfrom complex calculation" in {
      "4 3 * 2 / 1 -".calc === 5
    }
  }
}
