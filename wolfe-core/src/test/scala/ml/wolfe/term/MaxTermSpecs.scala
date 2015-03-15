package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author rockt
 */
class MaxTermSpecs extends WolfeSpec {
  import ml.wolfe.term.TermImplicits._

  "A max term" should {
    "evaluate to the maximum over its domain" in {
      val x = Bools.Var
      val term = max(Bools) { y => I(y && x)}
      term(x << false) should be(0.0)
      term(x << true) should be(1.0)
    }

    "provide an element of its sub-gradient" in {
      val weights = Vectors(2).Var
      val term = max(Bools) { label => I(label) * (weights dot vector(1, 2))}
      term.gradient(weights, vector(1, 1)) should equal(vector(1, 2))
      term.gradient(weights, vector(1, -1)) should equal(vector(0, 0))
    }

    "maximize over a structured search space" ignore {
      val labels = Discretes("V", "N")
      val sequences = Seqs(labels, 2)

      def model(y: sequences.DomTerm) =
        I(y(0) === labels.Const("V")) * 2.0 +
          I(y(1) === labels.Const("N")) * 1.0
      val result = max(sequences)(model)
      result.eval() should be(3.0)
    }

  }

}
