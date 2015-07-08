package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author rockt
 */
class LogZTermSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  "A logZ term" should {
    "evaluate to the maximum over its domain" in {
      val x = Bools.Var
      val term = logZ(Bools) { y => I(y && x)}
      term.eval(x := false) should be(math.log(math.exp(0) + math.exp(0)))
      term.eval(x := true) should be(math.log(math.exp(0) + math.exp(1)))
    }

    "provide an element of its gradient" in {
      val weights = Vectors(2).Var
      val term = logZ(Bools) { label => I(label) * (weights dot vector(1, 2))}
      val prob1 = math.exp(3) / (math.exp(3) + math.exp(0))
      val prob2 = math.exp(-1) / (math.exp(-1) + math.exp(0))
      term.diff(weights)(weights := vector(1, 1)) should equal(vector(1,2) * prob1)
      term.diff(weights)(weights := vector(1, -1)) should equal(vector(1,2) * prob2)
    }

//    "provide an element of its sub-gradient within a sum" in {
//      val weights = Vectors(2).Var
//      def model(label:BoolTerm) = I(label) * (weights dot vector(1, 2))
//      val term = model(true.toConst) - max(Bools) (model)
//      term.gradient(weights, vector(1, 1)) should equal(vector(0, 0))
//      term.gradient(weights, vector(1, -1)) should equal(vector(1, 2))
//    }


  }

}
