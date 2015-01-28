package ml.wolfe.fg20

/**
 * @author Sebastian Riedel
 */
trait WithObservation[P<:Potential] extends Potential {


  def observation:PartialSetting
  def self:P
  
  val fromThisToSelf = observation.fromObservedToAllMapping()

  val discVars = fromThisToSelf.tgtDisc.map(self.discVars)
  val contVars = fromThisToSelf.tgtCont.map(self.contVars)
  val vectVars = fromThisToSelf.tgtVect.map(self.vectVars)

  override def scorer() = new Scorer{
    val selfSetting = self.createPartialSetting()
    val selfScorer = self.scorer()

    observation.fillObserved(selfSetting)

    def score(setting:Setting) = {
      setting.copyTo(selfSetting,fromThisToSelf)
      selfScorer.score(selfSetting)
    }
  }
}

trait DifferentiableWithObservation extends WithObservation[Differentiable] with Differentiable {

  override def gradientCalculator() = new GradientCalculator {
    val selfGradientCalculator = self.gradientCalculator()
    val selfSetting = self.createPartialSetting()
    val selfGradient = self.createSetting()

    def gradientAndValue(currentParameters: PartialSetting, gradient: Setting) = {
      currentParameters.copyTo(selfSetting,fromThisToSelf)
      observation.copyObservedTo(selfSetting)
      val value = selfGradientCalculator.gradientAndValue(selfSetting,selfGradient)
      gradient.copyFrom(selfGradient,fromThisToSelf)
      value
    }
  }

}