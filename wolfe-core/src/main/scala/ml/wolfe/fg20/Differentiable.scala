package ml.wolfe.fg20

import ml.wolfe._

/**
 * Potentials which can be differentiated and which have a processor that
 * can provide the gradient at a given parameter.
 */
trait Differentiable extends Potential {
  def gradientCalculator(): GradientCalculator
}

/**
 * Convenience trait for potentials that require no states to calculate gradients.
 */
trait StatelessDifferentiable extends Differentiable with GradientCalculator {
  def gradientCalculator() = this
}


class LinearTerm(val x: ContVar, val scale: Double) extends ContPotential with StatelessDifferentiable with StatelessScorer {
  def contVars = Array(x)
  def gradientAndValue(current: PartialSetting, gradient: Setting) = {
    gradient.cont(0) = scale
    score(current)
  }
  def score(setting: Setting) = setting.cont(0) * scale
}

class QuadraticTerm(val x: ContVar, val scale: Double) extends ContPotential with StatelessDifferentiable with StatelessScorer  {
  val contVars = Array(x)
  def gradientAndValue(current: PartialSetting, gradient: Setting) = {
    gradient.cont(0) = current.cont(0) * 2.0 * scale
    score(current)
  }
  def score(setting: Setting) = setting.cont(0) * setting.cont(0) * scale
}

class BilinearTerm(val x1:ContVar, val x2:ContVar)

class MultivariateLinearTerm(val x: VectVar, val scale: FactorieVector) extends VectPotential
                                                                                with StatelessDifferentiable with StatelessScorer  {
  val vectVars = Array(x)
  def gradientAndValue(current: PartialSetting, gradient: Setting) = {
    gradient.vect(0) = scale
    score(current)
  }
  def score(setting: Setting) = setting.vect(0) dot scale
}

class MultivariateQuadraticTerm(val x: VectVar, val scale: Double) extends VectPotential
                                                                           with StatelessDifferentiable with StatelessScorer  {
  val vectVars = Array(x)
  def gradientAndValue(current: PartialSetting, gradient: Setting) = {
    gradient.vect(0) = current.vect(0) * scale * 2.0
    score(current)
  }
  def score(setting: Setting) = (setting.vect(0) dot setting.vect(0)) * scale
}


