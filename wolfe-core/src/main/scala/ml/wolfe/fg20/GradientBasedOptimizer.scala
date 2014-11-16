package ml.wolfe.fg20

import cc.factorie.la.{DenseTensor1, WeightsMapAccumulator}
import cc.factorie.model.{Weights, WeightsSet}
import cc.factorie.optimize.{AdaGrad, OnlineTrainer, Example, Trainer}
import cc.factorie.util.DoubleAccumulator
import ml.wolfe._

import scala.collection.mutable

/**
 * @author Sebastian Riedel
 */
class GradientBasedOptimizer(val problem: Problem) extends EdgeMsgsFG with EmptyFactorFG with EmptyNodeFG {
  class Msgs
  type DiscMsgs = Nothing
  class ContMsgs(var gradient: Double = 0.0) extends Msgs
  class VectMsgs(var gradient: FactorieVector = null) extends Msgs

  def createDiscMsgs(variable: DiscVar[Any]) = sys.error("Can't do gradient ascent with discrete variables")
  def createContMsgs(variable: ContVar) = new ContMsgs()
  def createVectMsgs(variable: VectVar) = new VectMsgs()

  type Pot = GradientBasedOptimizer.Potential
  def acceptPotential = { case p: GradientBasedOptimizer.Potential => p }

  def argmax(trainer: WeightsSet => Trainer = w => new OnlineTrainer(w,new AdaGrad(),100),
             init:State = State.empty):ArgmaxResult = {
    val weightsSet = new WeightsSet
    val weightsKeys = new mutable.HashMap[Node, Weights]()

    for (n <- contNodes) {
      weightsKeys(n) = weightsSet.newWeights(new DenseTensor1(Array(n.setting)))
    }
    for (n <- vectNodes) {
      n.setting = init.get(n.variable) match {
        case Some(v) => v
        case None => new DenseVector(n.variable.dim)
      }
      weightsKeys(n) = weightsSet.newWeights(n.setting)
    }
    val examples = for (f <- factors) yield new Example {
      val factor = f
      def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator) = {
        for (e <- f.contEdges) {
          e.node.setting = weightsSet(weightsKeys(e.node)).asInstanceOf[FactorieVector](0)
        }
        for (e <- f.vectEdges) {
          e.node.setting = weightsSet(weightsKeys(e.node)).asInstanceOf[FactorieVector]
        }
        val v = f.pot.gradientAndValue(f)
        for (e <- f.contEdges) {
          gradient.accumulate(weightsKeys(e.node),new DenseTensor1(Array(e.msgs.gradient)), 1.0)
        }
        for (e <- f.vectEdges) {
          gradient.accumulate(weightsKeys(e.node),e.msgs.gradient, 1.0)
        }

        value.accumulate(v)
      }
    }

    val learner = new ResamplingTrainer(this, trainer(weightsSet))
    learner.trainFromExamples(examples)
    //set results
    for (n <- contNodes) {
      n.setting = weightsSet(weightsKeys(n)).asInstanceOf[FactorieVector](0)
    }
    for (n <- vectNodes) {
      n.setting = weightsSet(weightsKeys(n)).asInstanceOf[FactorieVector]
    }
    val objective = factors.iterator.map(f => f.pot.score(f,new DenseVector(Array.empty[Double]))).sum
    val contState = contNodes.map(n => n.variable -> n.setting).toMap[Var[Any],Any]
    val vectState = vectNodes.map(n => n.variable -> n.setting).toMap[Var[Any],Any]
    ArgmaxResult(State(contState ++ vectState),objective)
  }

}

object GradientBasedOptimizer {
  trait Potential extends ml.wolfe.fg20.Potential {
    def gradientAndValue(factor: GradientBasedOptimizer#Factor):Double
  }
}

class ResamplingTrainer(fg:GradientBasedOptimizer, self:Trainer) extends Trainer {
  def processExamples(examples: Iterable[Example]) = {
    self.processExamples(examples)
    //fg.sampleFactors()
  }
  def isConverged = self.isConverged
}
