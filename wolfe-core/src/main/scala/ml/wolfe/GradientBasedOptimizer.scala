package ml.wolfe

import cc.factorie.WeightsSet
import cc.factorie.Weights
import cc.factorie.la.WeightsMapAccumulator
import cc.factorie.util.DoubleAccumulator
import cc.factorie.optimize._
import scala.language.reflectiveCalls
import scala.collection.mutable

/**
 * @author Sebastian Riedel
 */
object GradientBasedOptimizer {

  def apply(fg: FactorGraph, trainer: WeightsSet => Trainer) {
    //initialize all n2f messages with zero or some random value
    for (node <- fg.nodes) {
      node.variable.initializeRandomly(0.01)
    }
    val weightsSet = new WeightsSet
    val weightsKeys = new mutable.HashMap[FactorGraph.Node, Weights]()
    for (n <- fg.nodes) weightsKeys(n) = weightsSet.newWeights(new DenseVector(n.variable.asVector.dim))
    val examples = for (f <- fg.factors) yield new Example {
      val factor = f
      def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator) = {
        for (e <- f.edges) {
          e.msgs.asVector.n2f = weightsSet(weightsKeys(e.n)).asInstanceOf[FactorieVector]
        }
        val v = f.potential.valueAndGradientForAllEdges()
        for (e <- f.edges) {
          gradient.accumulate(weightsKeys(e.n), e.msgs.asVector.f2n, 1.0)
        }
        value.accumulate(v)
      }
    }

    val learner = trainer(weightsSet)
    learner.trainFromExamples(examples)


    //go over all factors, calculate gradients into f2n
    //collect all f2n messages into one gradient vector for factorie trainer
    //resample graph (hook in factorie?)

  }
}
