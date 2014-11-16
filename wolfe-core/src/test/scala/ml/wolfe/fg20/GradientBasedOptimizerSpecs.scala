package ml.wolfe.fg20

import cc.factorie.optimize.{AdaGrad, OnlineTrainer}
import ml.wolfe.{FactorieVector, WolfeSpec}

/**
 * @author Sebastian Riedel
 */
class GradientBasedOptimizerSpecs extends WolfeSpec {

  trait ContPotential extends GradientBasedOptimizer.Potential {
    def discVars = Array.empty
    def vectVars = Array.empty
    def isLinear = false
    def statsForCurrentSetting(factor: FG#Factor) = isNotLinear
  }

  class LinearTerm(val x:ContVar, val scale:Double) extends ContPotential {
    def contVars = Array(x)
    def gradientAndValue(factor: GradientBasedOptimizer#Factor) = {
      factor.contEdges(0).msgs.gradient = scale
      score(factor,null)
    }
    def score(factor: FG#Factor, weights: FactorieVector) = {
      factor.contEdges(0).node.setting * scale
    }
  }

  class QuadraticTerm(val x:ContVar, val scale:Double) extends ContPotential {
    def contVars = Array(x)
    def gradientAndValue(factor: GradientBasedOptimizer#Factor) = {
      factor.contEdges(0).msgs.gradient = factor.contEdges(0).node.setting * 2.0 * scale
      score(factor,null)
    }
    def score(factor: FG#Factor, weights: FactorieVector) = {
      factor.contEdges(0).node.setting * factor.contEdges(0).node.setting * scale
    }
  }


  "A gradient based optimizer" should {
    "optimize a quadratic objective" in {
      //f(x) = -x^2 + 4x
      val x = new ContVar("x")
      val problem = Problem(Seq(new QuadraticTerm(x,-1.0),new LinearTerm(x,4.0)),contVars = Seq(x))
      val optimizer = new GradientBasedOptimizer(problem)
      val result = optimizer.argmax(w => new OnlineTrainer(w,new AdaGrad(),1000))
      result.state(x) should be (2.0 +- 0.01)
    }
  }

}
