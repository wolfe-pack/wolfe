package ml.wolfe.term

import ml.wolfe.WolfeSpec

import scala.util.Random

/**
 * @author riedel
 */
class CachedTermSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  "A cached term" should {
    "return different value if input is different" in {
      var calls = 0
      val i = Ints(0 until 10).Var
      val t = cached(i map { x => calls += 1; x })
      val eval = t.evaluator()

      eval.eval(i := 1) should be(1)
      calls should be (1)
      eval.eval(i := 1) should be(1)
      calls should be (1)
      eval.eval(i := 2) should be(2)
      calls should be (2)

    }

  }

}
