package scalapplcodefest

import org.scalatest.{Matchers, FlatSpec}

/**
 * Set of specs.
 * @author Sebastian Riedel
 */
class Specs extends FlatSpec with Matchers {

  import TermImplicits._

  "An exhaustive argmaxer" should "find the maximizing assignment of a term" in {
    val x = 'x of Bools
    val y = 'y of Bools
    val model = I(x && !y)
    val argmax = Inference.exhaustiveArgmax(model).state()
    argmax(x) should be (true)
    argmax(y) should be (false)
  }

  "An MaxProduct argmaxer" should "find the maximizing assignment of a chain" in {
    val x = 'x of Bools
    val y = 'y of Bools
    val z = 'z of Bools
    val model = I(x |=> y) + I(y |=> z) + I(x)
    val expected = Inference.exhaustiveArgmax(model).state()
    val actual = Inference.maxProductArgmax(1)(model).state()
    actual should be (expected)

  }

}
