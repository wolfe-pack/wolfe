package ml.wolfe

import cc.factorie.la.SmartGradientAccumulator
import cc.factorie.model.{Weights, WeightsMap, WeightsSet}
import cc.factorie.optimize._
import cc.factorie.util.{GlobalLogging, LocalDoubleAccumulator}

import scala.collection.mutable

/**
 * @author Sebastian Riedel
 */
object Learn {

  def batch(optimizer: GradientOptimizer = new LBFGS())
           (weights: WeightsSet) =
    new BatchTrainer(weights, optimizer)

  def online(maxIterations: Int = 10, optimizer: GradientOptimizer = new AdaGrad())
            (weights: WeightsSet) = new WolfeOnlineTrainer(weights, optimizer, maxIterations)

}

/**
 * Learns the parameters of a model by computing the gradient and calling the
 * optimizer one example at a time.
 * @param weightsSet The parameters to be optimized
 * @param optimizer The optimizer
 * @param maxIterations The maximum number of iterations until reporting convergence
 * @param logEveryN After this many examples a log will be printed. If set to -1 10 logs will be printed.
 * @author Alexandre Passos
 */
class WolfeOnlineTrainer(val weightsSet: WeightsSet, val optimizer: GradientOptimizer = new AdaGrad, val maxIterations: Int = 3, var logEveryN: Int = -1) extends Trainer with GlobalLogging {
  var iteration        = 0
  val valueAccumulator = new LocalDoubleAccumulator
  override def processExamples(examples: Iterable[Example]): Unit = {
    if (logEveryN == -1) logEveryN = math.max(100, examples.size / 10)
    iteration += 1
    var valuesSeenSoFar = 0.0
    var timePerIteration = 0L
    var i = 0
    val iter = examples.iterator
    while (iter.hasNext) {
      val example = iter.next()
      val gradientAccumulator = new SmartGradientAccumulator
      if ((logEveryN != 0) && (i % logEveryN == 0) && (i != 0)) {
        logger.info(TrainerHelpers.getOnlineTrainerStatus(i, logEveryN, timePerIteration, valuesSeenSoFar))
        valuesSeenSoFar = 0.0
        timePerIteration = 0
      }
      val t0 = System.currentTimeMillis()
      gradientAccumulator.clear()
      valueAccumulator.value = 0
      example.accumulateValueAndGradient(valueAccumulator, gradientAccumulator)
      valuesSeenSoFar += valueAccumulator.value
      optimizer.step(weightsSet, gradientAccumulator.getMap, valueAccumulator.value)
      timePerIteration += System.currentTimeMillis() - t0
      i += 1
    }
  }
  def isConverged = iteration >= maxIterations
}

trait UnitBallProjection extends GradientStep {

  var weightsToNormalize = new mutable.HashSet[Weights]()

  override def doGradStep(weights: WeightsSet, gradient: WeightsMap, rate: Double) = {

    gradient.keys.foreach(k => {
      weights(k) +=(gradient(k), rate)
      if (weightsToNormalize(k)) {
        weights(k).normalize()
      }
    })

  }
}
