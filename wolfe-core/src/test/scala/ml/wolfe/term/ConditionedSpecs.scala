package ml.wolfe.term

import ml.wolfe.WolfeSpec

import scala.util.Random

/**
 * @author riedel
 */
class ConditionedSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  implicit val random = new Random(0)

  "A conditioned term" should {
    "evaluate to the inner term given a particular assignment to the given variable" in {
      val x = Doubles.Var
      val t = x + x
      val c = t | x << 1.0
      c.eval() should be (2.0)
    }

  }

}
