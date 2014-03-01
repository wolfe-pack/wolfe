package scalapplcodefest.legacy

import cc.factorie.optimize.{Trainer, Perceptron, OnlineTrainer, Example}
import cc.factorie.util.DoubleAccumulator
import cc.factorie.la.WeightsMapAccumulator
import cc.factorie.WeightsSet
import TermDSL._
import scalapplcodefest.legacy.term._
import scalapplcodefest._


/**
 * A minimizer of a differentiable real valued function. Uses factorie optimization package and
 * the [[scalapplcodefest.legacy.term.Differentiable]] pattern.
 */
object TrainerBasedMaximization {

  def minimize(weightVar:Variable[Vector], objective: Term[Double], trainerFor: WeightsSet => Trainer = new OnlineTrainer(_, new Perceptron, 5)) = {
    val weightsSet = new WeightsSet
    val key = weightsSet.newWeights(new DenseVector(10000))
    val instances = TermConverter.asSeq(objective, doubles.add)
    val examples = for (instance <- instances) yield {
      Differentiator.differentiate(weightVar, TermConverter.pushDownConditions(instance)) match {
        case Some(gradientTerm) =>
          new Example {
            def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator) = {
              val weights = weightsSet(key).asInstanceOf[Vector]
              val state = State(Map(weightVar -> weights))
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

trait ContinuousArgminHint extends CompilerHint {
  def minimize(param: Variable[Vector], objective: Term[Double]): Vector
}

case class GradientBasedArgminHint(trainerFor: WeightsSet => Trainer = new OnlineTrainer(_, new Perceptron, 5))
  extends ContinuousArgminHint {

  def minimize(param: Variable[Vector], objective: Term[Double]) = {
    val weightsSet = new WeightsSet
    val key = weightsSet.newWeights(new DenseVector(10000))
    val instances = TermConverter.asSeq(objective, doubles.add)
    val examples = for (instance <- instances) yield {
      TermConverter.pushDownConditions(instance) match {
        case Differentiable(p, gradientTerm) if p == param =>
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
