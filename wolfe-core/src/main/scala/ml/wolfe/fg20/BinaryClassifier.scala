package ml.wolfe.fg20

import cc.factorie.optimize.{AdaGrad, BatchTrainer, LBFGS}
import ml.wolfe._
import ml.wolfe.FactorieVector


/**
 * A simple logistic regression classifier
 * @param weights the weight vector.
 * @param feat the feature function.
 * @tparam X the type of observations to be mapped to reals.
 * @author msaeidi
 */
class BinaryClassifier[X](val weights: FactorieVector, val feat: X => FactorieVector) extends (X => Double) {
  def apply(x: X): Double = {
    val prob = getProbability(x)
    if (prob > 0.5) 1.0
    else 0.0
  }

  def getProbability(x: X): Double = {
    feat(x) dot weights
    val z = feat(x) dot weights
    val predict = 1.0 / (1 + math.exp(-z))
    predict
  }
}

/**
 * Negative L2 Norm loss (negative to allow maximization instead of minimization).
 * @param w weights variable
 * @param feats feature representation of instance.
 * @param y target.
 * @param scale a scale the term is multipled with.
 */
class L2NormCrossEntropyLoss(val w: VectVar, val feats: FactorieVector,
                             val y: Double, val scale: Double = 1.0) extends StatelessDifferentiable with StatelessScorer
                                                                             with VectPotential {
  val vectVars = Array(w)

  def gradientAndValue(currentParameters: PartialSetting, gradient: Setting) = {
    val z = feats dot currentParameters.vect(0)
    val guess = 1.0 / (1 + math.exp(-z))
    gradient.vect(0) = feats * (scale * (y - guess))
    score(currentParameters)
  }
  def score(setting: Setting) = {
    val z = feats dot setting.vect(0)
    val guess = 1.0 / (1 + math.exp(-z))
    scale * -(y * math.log(guess) + (1 - y) * math.log(1 - guess))
  }
}

///**
// * Negative L2 Regularizer loss (negative to allow maximization instead of minimization).
// * @param w weights variable
// * @param scale a scale the term is multiplied with.
// */
//class L2Regularizer(val w: VectVar, val scale: Double = 1.0) extends StatelessDifferentiable with StatelessScorer
//                                                                     with VectPotential {
//  val vectVars = Array(w)
//
//  def gradientAndValue(currentParameters: PartialSetting, gradient: Setting) = {
//    gradient.vect(0) = currentParameters.vect(0) * (-2.0 * scale)
//    score(currentParameters)
//  }
//  def score(setting: Setting) = {
//    -scale * setting.vect(0).twoNormSquared
//  }
//}


/**
 * Companion object for regressors.
 */
object BinaryClassifier {

  /**
   * Learns a logistic regression classifier from data.
   * @param data pairs of observations and target values.
   * @param reg regularization parameter.
   * @param feat feature function.
   * @tparam X type of source elements.
   * @return a logistic regression classifier.
   */
  def learn[X](data: Seq[(X, Double)], reg: Double = 0.1)(feat: X => FactorieVector): BinaryClassifier[X] = {
    val instances = data.toList.map({ case (x, y) => y -> feat(x) })
    val maxDim = instances.iterator.map(_._2.dim1).max
    val weightsVar = new VectVar(maxDim, "w")
    val loss = for ((y, f) <- instances) yield new L2NormCrossEntropyLoss(weightsVar, f, y)
    val problem = Problem(new L2Regularizer(weightsVar, reg) :: loss)
    val optimizer = new GradientBasedOptimizer(problem)
    val result = optimizer.gradientBasedArgmax(new BatchTrainer(_, new AdaGrad(), 100))
    new BinaryClassifier(result.state(weightsVar), feat)
  }

}

