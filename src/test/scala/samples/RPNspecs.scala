package samples

import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner._
import scala.util.{Success, Failure}
import io.jorand.kata.RPN._
import scala.util.Success


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
    "handle simple case" in {
      "2 3 +".calc === Success(5)
    }
    "handle simple negative case" in {
      "2 3 -".calc === Success(-1)
    }
    "handle complex case" in {
      "30 3 + 11 / 4 * 6 -".calc === Success(6)
    }

    "handle bad operator" in {
      "30 3 $ 11 / 4 * 6 -".calc must not be Success(0)
    }

  }
}
