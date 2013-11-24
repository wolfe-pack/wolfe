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
    val term = I(x && y)
    val argmax = Inference.exhaustiveArgmax(term)
    argmax(x) should be (true)
    argmax(y) should be (true)
  }

}
