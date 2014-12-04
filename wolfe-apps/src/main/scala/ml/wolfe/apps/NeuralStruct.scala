package ml.wolfe.apps

import ml.wolfe.fg20._

/**
 * @author Sebastian Riedel
 */
class NeuralStruct {

  //minimize loss Anns(\theta,scores) + Hinge(scores)
  //use proximal gradient method, use gradient of ANNs, and proximal operator of Hinge (minimize hinge loss + L2 regularizer)
  //initialize scores with -2/2 for gold edges/non-edges.

}

trait ADMMProcessor extends Scorer {
  def argmaxWithL2Reg(incomingReg: Msgs, partial: PartialSetting, result: Setting): Double
}

class ScorerLoss(val perInstanceScores: List[VectVar], weights: VectVar) extends VectPotential {
  val vectVars = (weights :: perInstanceScores).toArray
  def instanceIndex(instance: Int) = 1 + instance
  class Proc extends ADMMProcessor {
    def score(setting: Setting) = {
      0.0
    }
    def argmaxWithL2Reg(incomingReg: Msgs, partial: PartialSetting, result: Setting) = ???
  }
  def scorer() = ???
}

class TotalStructuredLoss(val perInstanceScores: List[VectVar]) extends VectPotential {
  val vectVars = perInstanceScores.toArray
  //processor needs to be able to argmaxWithL2regularizer (in incoming message)
  class Proc extends Scorer {
    def score(setting: Setting) = ???
  }
  def scorer() = ???
}

class PerSentenceStructuredLoss(perInstanceScore: VectVar) extends Differentiable with VectPotential {

  val vectVars = Array(perInstanceScore)

  val treePot = new ProjectiveTreePotential(10, null, ???)

  class Proc extends GradientCalculator with Scorer {
    def gradientAndValue(currentParameters: Setting, gradient: Setting) = ???
    def score(setting: Setting) = ???
  }

  def scorer() = new Proc
  def gradientCalculator = new Proc
}

class PerSentenceScorerLoss(perInstanceScore: VectVar, weights: VectVar) {

}

class ProjectiveTreePotentialHingeLoss() extends VectPotential {
  def vectVars = ???
  def scorer() = new Proc
  class Proc extends ADMMProcessor {

    def argmaxWithL2Reg(incomingReg: Msgs, partial: PartialSetting, result: Setting) =
      ???
    def gradientAndValue(currentParameters: Setting, gradient: Setting) = {
      //
      ???
    }

    def score(setting: Setting) = ???
  }
}

trait ProximalOperator {
  def prox(lambda: Double, incoming: Setting, partial: PartialSetting, result: Setting)
}

trait ProximalPotential extends Potential { type Proc <: ProximalOperator }

//http://stanford.edu/~boyd/papers/pdf/prox_slides.pdf
//do it online http://jmlr.csail.mit.edu/proceedings/papers/v28/suzuki13.pdf
//online mirror descent?
//online regularized dual ascent?
class ProximalGradientMethod(val smoothF: Differentiable, val convexG: ProximalPotential) {

  //get shared variable
  //create processors

}


