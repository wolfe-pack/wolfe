package ml.wolfe.fg20

import ml.wolfe.FactorieVector
import ml.wolfe.term.Setting

/**
 * @author Sebastian Riedel
 */
class LinearClassiferPotential[T](labelSpace: AtomicDomain.Disc[T],
                                  weightSpace: AtomicDomain.Vect,
                                  features: T => FactorieVector) extends Potential
                                                                         with SupportsArgmax
                                                                         with Differentiable {
  val discVars:Array[DiscVar[Any]] = Array(labelSpace.variable)
  val contVars = Potential.emptyContVars
  val vectVars = Array(weightSpace.variable)
  val numLabels = labelSpace.variable.dom.size

  trait Processor {
    val feats = labelSpace.variable.dom.map(features).toArray
    final def scoreLabel(weights:FactorieVector, l:Int) = {
      feats(l) dot weights
    }
  }

  def scorer() = new Scorer with Processor {
    def score(setting: Setting) = {
      val label = setting.disc(0)
      val weights = setting.vect(0)
      scoreLabel(weights,label)
    }
  }

  def argmaxer() = new Argmaxer with Processor {
    def argmax(observed: PartialSetting, incoming: Msgs, result: Setting, score: DoubleBuffer) = {
      require(observed.vectObs(0), "Can only argmax when weights are observed")
      val weights = observed.vect(0)
      var max = Double.NegativeInfinity
      var maxIndex = -1
      for (l <- 0 until numLabels) {
        val score = scoreLabel(weights,l) + incoming.disc(0).msg(l)
        if (score > max) {
          max = score
          maxIndex = l
        }
      }
      score.value = max
      result.disc(0) = maxIndex
    }
  }
  def gradientCalculator() = new GradientCalculator with Processor {

    def gradientAndValue(currentParameters: PartialSetting, gradient: Setting) = {
      require(currentParameters.discObs(0), "Can only calculate gradients when label is observed")
      //gradient is feature vector at label
      val label = currentParameters.disc(0)
      val weights = currentParameters.vect(0)
      gradient.vect(0) = feats(label)
      scoreLabel(weights,label)
    }
  }
}

