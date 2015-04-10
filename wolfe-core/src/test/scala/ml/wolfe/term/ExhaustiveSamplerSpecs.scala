package ml.wolfe.term

import ml.wolfe.WolfeSpec

import scala.util.Random

/**
 * @author riedel
 */
class ExhaustiveSamplerSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  implicit val random = new Random(0)

  "A sample term" should {
    "sample from a deterministic distribution" in {
      sample(Bools)(x => I(x)).eval() should be(true)
      sample(Bools)(x => 1.0 - I(x)).eval() should be(false)
    }

    "sample from a uniform distribution" in {
      val n = Ints(0 until 10000).Var
      val expect = sum(0 until n) { _ => I(sample(Bools)(_ => 1.0)) } / n
      expect.eval(n := 1000) should be (0.5 +- 0.1)

      //      sample(Bools)(x => I(x)).eval() should be (true)
      //      sample(Bools)(x => 1.0 - I(x)).eval() should be (false)
    }


  }

}
