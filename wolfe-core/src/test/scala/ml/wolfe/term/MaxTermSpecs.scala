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
      term.eval(x := false) should be(0.0)
      term.eval(x := true) should be(1.0)
    }

    "provide an element of its sub-gradient" in {
      val weights = Vectors(2).Var
      val term = max(Bools) { label => I(label) * (weights dot vector(1, 2))}
      term.gradient(weights, vector(1, 1)) should equal(vector(1, 2))
      term.gradient(weights, vector(1, -1)) should equal(vector(0, 0))
    }

    "provide an element of its sub-gradient within a sum" in {
      val weights = Vectors(2).Var
      def model(label:BoolTerm) = I(label) * (weights dot vector(1, 2))
      val term = model(true.toConst) - max(Bools) (model)
      term.gradient(weights, vector(1, 1)) should equal(vector(0, 0))
      term.gradient(weights, vector(1, -1)) should equal(vector(1, 2))
    }

    "maximize over a structured search space" in {
      implicit val Labels = Discretes("V", "N")
      val Sequences = Seqs(Labels, 2)

      def model(y: Sequences.Term) = I(y(0) === "V") * 2.0 + I(y(1) === "N") * 1.0
      val result = max(Sequences)(model)
      result.eval() should be(3.0)
    }

  }

}
