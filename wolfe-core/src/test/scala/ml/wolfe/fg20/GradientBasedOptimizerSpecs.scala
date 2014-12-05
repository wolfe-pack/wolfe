package ml.wolfe.fg20

import cc.factorie.la.DenseTensor1
import cc.factorie.optimize.{BatchTrainer, AdaGrad, OnlineTrainer}
import ml.wolfe.{FactorieVector, WolfeSpec}

/**
 * @author Sebastian Riedel
 */
class GradientBasedOptimizerSpecs extends WolfeSpec {


  class LinearTerm(val x: ContVar, val scale: Double) extends ContPotential with StatelessDifferentiable with StatelessScorer {
    def contVars = Array(x)
    def gradientAndValue(current: Setting, gradient: Setting) = {
      gradient.cont(0) = scale
      score(current)
    }
    def score(setting: Setting) = setting.cont(0) * scale
  }

  class QuadraticTerm(val x: ContVar, val scale: Double) extends ContPotential with StatelessDifferentiable with StatelessScorer  {
    val contVars = Array(x)
    def gradientAndValue(current: Setting, gradient: Setting) = {
      gradient.cont(0) = current.cont(0) * 2.0 * scale
      score(current)
    }
    def score(setting: Setting) = setting.cont(0) * setting.cont(0) * scale
  }

  class MultivariateLinearTerm(val x: VectVar, val scale: FactorieVector) extends VectPotential
                                                                                  with StatelessDifferentiable with StatelessScorer  {
    val vectVars = Array(x)
    def gradientAndValue(current: Setting, gradient: Setting) = {
      gradient.vect(0) = scale
      score(current)
    }
    def score(setting: Setting) = setting.vect(0) dot scale
  }

  class MultivariateQuadraticTerm(val x: VectVar, val scale: Double) extends VectPotential
                                                                             with StatelessDifferentiable with StatelessScorer  {
    val vectVars = Array(x)
    def gradientAndValue(current: Setting, gradient: Setting) = {
      gradient.vect(0) = current.vect(0) * scale * 2.0
      score(current)
    }
    def score(setting: Setting) = (setting.vect(0) dot setting.vect(0)) * scale
  }


  "A gradient based optimizer" should {
    "optimize a univariate quadratic objective" in {
      //f(x) = -x^2 + 4x  = -(2 - x)^2 + c
      val x = new ContVar("x")
      val problem = Problem(Seq(new QuadraticTerm(x, -1.0), new LinearTerm(x, 4.0)))
      val optimizer = new GradientBasedOptimizer(problem)
      val result = optimizer.argmax(w => new OnlineTrainer(w, new AdaGrad(), 1000))
      result.state(x) should be(2.0 +- 0.01)
    }

    "optimize a multivariate quadratic objective" in {
      val x = new VectVar(2, "x")
      val problem = Problem(Seq(
        new MultivariateLinearTerm(x, new DenseTensor1(Array(4.0, 0.0))),
        new MultivariateQuadraticTerm(x, -1.0))
      )
      val optimizer = new GradientBasedOptimizer(problem)
      val result = optimizer.argmax(w => new OnlineTrainer(w, new AdaGrad(), 10000))
      result.state(x)(0) should be(2.0 +- 0.01)
      result.state(x)(1) should be(0.0 +- 0.01)
    }
    "optimize a multivariate quadratic objective in batch mode" in {
      val x = new VectVar(2, "x")
      val problem = Problem(Seq(
        new MultivariateLinearTerm(x, new DenseTensor1(Array(4.0, 0.0))),
        new MultivariateQuadraticTerm(x, -1.0))
      )
      val optimizer = new GradientBasedOptimizer(problem)
      val result = optimizer.argmax(w => new BatchTrainer(w, new AdaGrad(), 100))
      result.state(x)(0) should be(2.0 +- 0.01)
      result.state(x)(1) should be(0.0 +- 0.01)
    }

  }


}
