package scalapplcodefest

import cc.factorie.optimize.{Trainer, Perceptron, OnlineTrainer, Example}
import cc.factorie.util.DoubleAccumulator
import cc.factorie.la.WeightsMapAccumulator
import cc.factorie.WeightsSet
import TermImplicits._


/**
 * A minimizer of a differentiable real valued function. Uses factorie optimization package and
 * the [[scalapplcodefest.Differentiable]] pattern.
 */
object GradientBasedMinimizer {
  def minimize(objective: Term[Double], trainerFor: WeightsSet => Trainer = new OnlineTrainer(_, new Perceptron, 5)) = {
    val weightsSet = new WeightsSet
    val key = weightsSet.newWeights(new DenseVector(10000))
    val instances = TermConverter.asSeq(objective, doubles.add)
    val examples = for (instance <- instances) yield {
      TermConverter.pushDownConditions(instance) match {
        case Differentiable(param,gradientTerm) =>
          new Example {
            def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator) = {
              val weights = weightsSet(key).asInstanceOf[Vector]
              val state = State(Map(param -> weights))
              val v = instance.value(state)
              val g = gradientTerm.value(state)
              value.accumulate(v)
              gradient.accumulate(key, g, -1.0)
            }
          }
        case _ => sys.error("Can't differentiate " + instance)
      }
    }
    val trainer = trainerFor(weightsSet)
    trainer.trainFromExamples(examples)
    weightsSet(key).asInstanceOf[Vector]


  }
}
