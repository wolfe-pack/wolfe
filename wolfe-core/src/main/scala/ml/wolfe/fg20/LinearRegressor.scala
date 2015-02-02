package ml.wolfe.fg20

import cc.factorie.optimize.{BatchTrainer, LBFGS}
import ml.wolfe.FactorieVector
import ml.wolfe.term.Setting


/**
 * A simple linear regressor
 * @param weights the weight vector.
 * @param feat the feature function.
 * @tparam X the type of observations to be mapped to reals.
 * @author Sebastian Riedel
 */
class LinearRegressor[X](val weights: FactorieVector, val feat: X => FactorieVector) extends (X => Double) {
  def apply(x: X): Double = feat(x) dot weights
}

/**
 * Negative L2 Norm loss (negative to allow maximization instead of minimization).
 * @param w weights variable
 * @param feats feature representation of instance.
 * @param y target.
 * @param scale a scale the term is multipled with.
 */
class L2NormLoss(val w: VectVar, val feats: FactorieVector,
                 val y: Double, val scale: Double = 1.0) extends StatelessDifferentiable with StatelessScorer
                                                                 with VectPotential {
  val vectVars = Array(w)

  def gradientAndValue(currentParameters: PartialSetting, gradient: Setting) = {
    val guess = score(currentParameters)
    gradient.vect(0) = feats * (scale * 2.0 * (y - guess))
    guess
  }
  def score(setting: Setting) = {
    val guess = feats dot setting.vect(0)
    scale * -(y - guess) * (y - guess)
  }
}

/**
 * Negative L2 Regularizer loss (negative to allow maximization instead of minimization).
 * @param w weights variable
 * @param scale a scale the term is multiplied with.
 */
class L2Regularizer(val w: VectVar, val scale: Double = 1.0) extends StatelessDifferentiable with StatelessScorer
                                                                     with VectPotential {
  val vectVars = Array(w)

  def gradientAndValue(currentParameters: PartialSetting, gradient: Setting) = {
    gradient.vect(0) = currentParameters.vect(0) * (-2.0 * scale)
    score(currentParameters)
  }
  def score(setting: Setting) = {
    -scale * setting.vect(0).twoNormSquared
  }
}


/**
 * Companion object for regressors.
 */
object LinearRegressor {

  /**
   * Learns a linear regressor from data.
   * @param data pairs of observations and target values.
   * @param reg regularization parameter.
   * @param feat feature function.
   * @tparam X type of source elements.
   * @return a linear regressor.
   */
  def learn[X](data: Seq[(X, Double)], reg: Double = 0.1)(feat: X => FactorieVector): LinearRegressor[X] = {
    val instances = data.toList.map({ case (x, y) => y -> feat(x) })
    val maxDim = instances.iterator.map(_._2.dim1).max
    val weightsVar = new VectVar(maxDim, "w")
    val loss = for ((y, f) <- instances) yield new L2NormLoss(weightsVar, f, y)
    val problem = Problem(new L2Regularizer(weightsVar, reg) :: loss)
    val optimizer = new GradientBasedOptimizer(problem)
    val result = optimizer.gradientBasedArgmax(new BatchTrainer(_, new LBFGS(), 100))
    new LinearRegressor(result.state(weightsVar), feat)
  }

}
