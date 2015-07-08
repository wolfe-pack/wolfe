package ml.wolfe.term

import ml.wolfe.WolfeSpec
import ml.wolfe.util.Math._

/**
 * @author rockt
 */
class DiscreteDomSpecs extends WolfeSpec {
  import ml.wolfe.term.TermImplicits._

  "A discrete domain" should {
    "calculate hamming distances" in {
      val Elems = Seq('A, 'B, 'C).toDom
      val x = Elems.Var
      val y = Elems.Var

      Elems.hamming(x,y).eval(x := 'A, y := 'B) should be (1.0)
      Elems.hamming(x,y).eval(x := 'A, y := 'A) should be (0.0)

    }
  }

}
