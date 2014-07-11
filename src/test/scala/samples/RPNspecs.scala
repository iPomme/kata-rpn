package samples

import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner._
import scala.util.Success
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
    "resolve simple calculation" in {
      "2 3 +".calc === Success(5)
    }
    "resolve simple negative calculation" in {
      "2 -3 +".calc === Success(-1)
    }
    "resolve complex calculation" in {
      "2 3 + 5 * 10 / 2 -".calc === Success(0.5)
    }
  }
}
