package ml.wolfe.fg20

/**
 * @author Sebastian Riedel
 */
trait WithObservation[P<:Potential] extends ProxyPotential[P] {


  def observation:PartialSetting

  override def scorer() = new Scorer{
    val selfScorer = self.scorer()
    def score(setting:Setting) = {
      setting.fillObserved(observation)
      selfScorer.score(setting)
    }
  }
}

trait DifferentiableWithObservation extends WithObservation[Differentiable] with Differentiable {

  override def gradientCalculator() = new GradientCalculator {
    val selfGradientCalculator = self.gradientCalculator()

    def gradientAndValue(currentParameters: PartialSetting, gradient: Setting) = {
      currentParameters.fillObserved(observation)
      selfGradientCalculator.gradientAndValue(currentParameters,gradient)
    }
  }

}