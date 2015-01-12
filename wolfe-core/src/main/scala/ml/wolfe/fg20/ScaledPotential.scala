package ml.wolfe.fg20

/**
 * @author Sebastian Riedel
 */
trait ScaledPotential[P <: Potential] extends ProxyPotential[P] {

  def scale:Double

  def scorer() = new Scorer {

    val selfScorer = self.scorer()

    def score(setting: Setting) = {
      scale * selfScorer.score(setting)
    }
  }

}

object ScaledPotential {
  def scale[P <: Potential](p:P, scaleToUse:Double) = new ScaledPotential[P] {
    def scale = scaleToUse
    def self = p
  }

  def scaleDifferentiable[P <: Differentiable](p:P, scaleToUse:Double) = new ScaledDifferentable[P] {
    def scale = scaleToUse
    def self = p
  }

}

trait ScaledDifferentable[P <: Differentiable] extends ScaledPotential[P] with Differentiable {
  def gradientCalculator = new GradientCalculator {

    val selfGradientCalculator = self.gradientCalculator()

    def gradientAndValue(currentParameters: PartialSetting, gradient: Setting) = {
      val result = selfGradientCalculator.gradientAndValue(currentParameters, gradient)
      //now scale gradient
      gradient *= scale
      scale * result
    }
  }
}

trait ScaledSupportsArgmax[P <: SupportsArgmax] extends ScaledPotential[P] with SupportsArgmax {
  def argmaxer(): Argmaxer = new Argmaxer {
    val selfArgmaxer = self.argmaxer()
    def argmax(observed: PartialSetting, incoming: Msgs, result: Setting, score: DoubleBuffer): Unit = {
      require(scale > 0.0, "Scale needs to be positive to support argmax")
      selfArgmaxer.argmax(observed,incoming,result,score)
      score.value = scale * score.value
    }
  }
}


trait ProxyPotential[P <: Potential] extends Potential {
  def self:P
  def discVars = self.discVars
  def contVars = self.contVars
  def vectVars = self.vectVars
}
