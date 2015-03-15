package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class IntTermSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  "A int term" should {
    "have a singleton domain if its a constant" in {
      val i:IntTerm = 1
      i.domain.values should be (1 until 2)
    }

    "have an increased range if constant is added" in {
      val i = Ints(0 until 4).Var
      val t = i + 2
      t.domain.values should be (2 until 6)
    }

    "have a decreased range if constant is subtracted" in {
      val i = Ints(0 until 4).Var
      val t = i - 2
      t.domain.values should be (-2 until 2)
    }


  }


}
