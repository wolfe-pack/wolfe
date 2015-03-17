package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class AdagradSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  "Adagrad" should {
    "optimize a quadratic objective" in {
      val x = Doubles.Var
      def obj(x: Doubles.Term) = x * 4.0 - x * x
      val result = argmax(Doubles) { x => obj(x).argmaxBy(Argmaxer.adaGrad(AdaGradParameters(100, 1)))}
      result.eval() should be(2.0 +- eps)
    }
    "optimize a multivariate quadratic objective" in {
      val X = Vectors(2)
      val x = X.Var
      def obj(x: X.Term) = (x dot vector(4.0, 4.0)) - (x dot x)
      val result = argmax(X) { x => obj(x).argmaxBy(Argmaxer.adaGrad(AdaGradParameters(100, 1)))}
      result.eval() should equal(vector(2.0, 2.0))
    }
  }


}
