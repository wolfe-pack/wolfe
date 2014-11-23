package ml.wolfe.fg20

import cc.factorie.la.DenseTensor1
import cc.factorie.optimize.{AdaGrad, OnlineTrainer}
import ml.wolfe.{FactorieVector, WolfeSpec}

/**
 * @author Sebastian Riedel
 */
class GradientBasedOptimizerSpecs extends WolfeSpec {

  trait ContPotential extends GradientBasedOptimizer.Potential
                              with GradientBasedOptimizer.Potential2
                              with GradientBasedOptimizer.Processor
                              with StatelessProcessor[ContPotential] {
    def discVars = Array.empty
    def vectVars = Array.empty
    def isLinear = false
    def statsForCurrentSetting(factor: FG#Factor) = isNotLinear
//    def gradientAndValue(current: Setting, gradient: Setting) = ???
//    def score(setting: Setting) = ???
  }

  trait VectPotential extends GradientBasedOptimizer.Potential
                              with GradientBasedOptimizer.Potential2
                              with GradientBasedOptimizer.Processor
                              with StatelessProcessor[VectPotential]{
    def discVars = Array.empty
    def contVars = Array.empty
    def isLinear = false
    def statsForCurrentSetting(factor: FG#Factor) = isNotLinear
  }

  class LinearTerm(val x: ContVar, val scale: Double) extends ContPotential  {
    def contVars = Array(x)
    def gradientAndValue(factor: GradientBasedOptimizer#Factor) = {
      factor.contEdges(0).msgs.gradient = scale
      score(factor, null)
    }
    def score(factor: FG#Factor, weights: FactorieVector) = {
      factor.contEdges(0).node.setting * scale
    }
    def gradientAndValue(current: Setting, gradient: Setting) = {
      gradient.cont(0) = scale
      score(current)
    }
    def score(setting: Setting) = setting.cont(0) * scale
  }

  class QuadraticTerm(val x: ContVar, val scale: Double) extends ContPotential {
    def contVars = Array(x)
    def gradientAndValue(factor: GradientBasedOptimizer#Factor) = {
      factor.contEdges(0).msgs.gradient = factor.contEdges(0).node.setting * 2.0 * scale
      score(factor, null)
    }
    def score(factor: FG#Factor, weights: FactorieVector) = {
      factor.contEdges(0).node.setting * factor.contEdges(0).node.setting * scale
    }
    def gradientAndValue(current: Setting, gradient: Setting) = {
      gradient.cont(0) = current.cont(0) * 2.0 * scale
      score(current)
    }
    def score(setting: Setting) = setting.cont(0) * setting.cont(0) * scale
  }


  class MultivariateLinearTerm(val x: VectVar, val scale: FactorieVector) extends VectPotential {
    def vectVars = Array(x)
    def gradientAndValue(factor: GradientBasedOptimizer#Factor) = {
      factor.vectEdges(0).msgs.gradient = scale
      score(factor, null)
    }
    def score(factor: FG#Factor, weights: FactorieVector) = {
      factor.vectEdges(0).node.setting dot scale
    }
    def gradientAndValue(current: Setting, gradient: Setting) = ???
    def score(setting: Setting) = ???
  }

  class MultivariateQuadraticTerm(val x: VectVar, val scale: Double) extends VectPotential {
    def vectVars = Array(x)
    def gradientAndValue(factor: GradientBasedOptimizer#Factor) = {
      factor.vectEdges(0).msgs.gradient = factor.vectEdges(0).node.setting * scale * 2.0
      score(factor, null)
    }
    def score(factor: FG#Factor, weights: FactorieVector) = {
      (factor.vectEdges(0).node.setting dot factor.vectEdges(0).node.setting) * scale
    }
    def gradientAndValue(current: Setting, gradient: Setting) = ???
    def score(setting: Setting) = ???
  }


  "A gradient based optimizer" should {
    "optimize a univariate quadratic objective" in {
      //f(x) = -x^2 + 4x
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
      result.state(x)(0) should be (2.0 +- 0.01)
      result.state(x)(1) should be (0.0 +- 0.01)
    }
  }


}
