package ml.wolfe.fg20

/**
 * A potential that returns the maximum of another potential.
 * @author Sebastian Riedel
 */
trait MaxPotential[P <: SupportsArgmax] extends Potential {
  def notOptimized: Clique
  def objective: P

  //create mapping from objective clique to max clique
  val withoutDomainToWithDomain = new ArgMap(
    notOptimized.discVars.map(v => objective.discVars.indexOf(v)),
    notOptimized.contVars.map(v => objective.contVars.indexOf(v)),
    notOptimized.vectVars.map(v => objective.vectVars.indexOf(v)))
  def discVars = notOptimized.discVars
  def contVars = notOptimized.contVars
  def vectVars = notOptimized.vectVars

  trait Processor {
    val objectiveSetting = objective.createPartialSetting()
    val incoming         = objective.createMsgs()
    val result           = objective.createPartialSetting()
    val argmaxer         = objective.argmaxer()
    val max              = new DoubleBuffer()
  }

  def scorer() = new Scorer with Processor {
    def score(setting: Setting) = {
      setting.copyTo(objectiveSetting, withoutDomainToWithDomain)
      setting.observeIn(objectiveSetting, withoutDomainToWithDomain)
      argmaxer.argmax(objectiveSetting, incoming, result, max)
      max.value
    }
  }
}

trait DifferentiableMaxPotential[P <: Differentiable with SupportsArgmax] extends MaxPotential[P] with Differentiable {
  def gradientCalculator() = new GradientCalculator with Processor {
    val objGradientCalculator = objective.gradientCalculator()
    val objGradient           = objective.createSetting()
    def gradientAndValue(currentParameters: PartialSetting, gradient: Setting) = {

      //get argmax at setting
      currentParameters.copyTo(objectiveSetting, withoutDomainToWithDomain)
      currentParameters.observeIn(objectiveSetting, withoutDomainToWithDomain)
      argmaxer.argmax(objectiveSetting, incoming, result, max)

      //add the potential parameters to the result to appear for gradient calculation.
      currentParameters.copyTo(result, withoutDomainToWithDomain)

      //make sure resulting argmax value is observed in the objective todo: this could be done outside of this method
      currentParameters.inverseObserveIn(result, withoutDomainToWithDomain, true)

      //now get gradient at objective with given argmax setting fixed.
      objGradientCalculator.gradientAndValue(result, objGradient)

      //copy resulting gradient  into the actual result
      gradient.copyFrom(objGradient, withoutDomainToWithDomain)
      max.value
    }
  }
}