package ml.wolfe

import cc.factorie.WeightsSet
import cc.factorie.optimize._

/**
 * @author Sebastian Riedel
 */
object Learn {

  def batch(optimizer:GradientOptimizer = new LBFGS())
           (weights:WeightsSet) =
    new BatchTrainer(weights, optimizer)

  def online(maxIterations:Int = 10, optimizer:GradientOptimizer = new AdaGrad())
            (weights:WeightsSet) = new OnlineTrainer(weights, optimizer, maxIterations)

}
