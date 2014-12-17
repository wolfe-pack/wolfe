package ml.wolfe.fg20

import cc.factorie.la.{DenseTensor1, WeightsMapAccumulator}
import cc.factorie.model.{Weights, WeightsSet}
import cc.factorie.optimize.{AdaGrad, Example, OnlineTrainer, Trainer}
import cc.factorie.util.DoubleAccumulator
import ml.wolfe._

import scala.collection.mutable


/**
 * A processor that can calculate gradients of potentials.
 */
trait GradientCalculator {
  /**
   * Calculate value and gradient and current setting. Gradient results are
   * to be provided in-place by modifying the gradient argument.
   * @param currentParameters the current parameters, i.e. assignment to the potential's variables. If a setting
   *                          is observed no gradient needs to be calculated.
   * @param gradient put the gradient at the current parameters into this setting.
   * @return the value at the current parameters.
   */
  def gradientAndValue(currentParameters: PartialSetting, gradient: Setting): Double

}


/**
 * Create an optimizer for the given problem.
 * @param problem the problem to optimize.
 */
class GradientBasedOptimizer(val problem: Problem[Differentiable]) extends EmptyEdgeFactorGraph
                                                                           with EmptyNodeFactorGraph {

  type Pot = Differentiable
  type Processor = GradientCalculator
  def processor(pot: Pot) = pot.gradientCalculator()

  /**
   * Optimize the objective and return the argmax state and value.
   * @param trainer the trainer is the factorie optimizer this class uses internally.
   * @param init the initial parameter set.
   * @return an argmax result.
   */
  def gradientBasedArgmax(trainer: WeightsSet => Trainer = w => new OnlineTrainer(w, new AdaGrad(), 100),
                          init: State = State.empty,
                          stochastic: Seq[Differentiable] = Seq.empty): ArgmaxResult = {


    this.stochastic = stochastic.map(pot2Factor)
    val weightsSet = new WeightsSet
    val weightsKeys = new mutable.HashMap[Node, Weights]()

    for (n <- contNodes) {
      n.setting = init.get(n.variable) match {
        case Some(v) => v
        case None => 0.0
      }
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

        for (e <- f.contEdges; if !e.node.observed) {
          f.setting.cont(e.index) = weightsSet(weightsKeys(e.node)).asInstanceOf[FactorieVector](0)
        }
        for (e <- f.vectEdges; if !e.node.observed) {
          f.setting.vect(e.index) = weightsSet(weightsKeys(e.node)).asInstanceOf[FactorieVector]
        }
        val v = f.processor.gradientAndValue(f.setting, f.gradient)
        for (e <- f.contEdges; if !e.node.observed) {
          gradient.accumulate(weightsKeys(e.node), new DenseTensor1(Array(f.gradient.cont(e.index))), 1.0)
        }
        for (e <- f.vectEdges; if !e.node.observed) {
          gradient.accumulate(weightsKeys(e.node), f.gradient.vect(e.index), 1.0)
        }

        value.accumulate(v)
      }
    }

    val learner = new ResamplingTrainer(this, trainer(weightsSet))
    learner.trainFromExamples(examples)
    //set results
    for (n <- contNodes) {
      n.setting = weightsSet(weightsKeys(n)).asInstanceOf[FactorieVector](0)
      for (e <- n.edges) e.factor.setting.cont(e.index) = n.setting
    }
    for (n <- vectNodes) {
      n.setting = weightsSet(weightsKeys(n)).asInstanceOf[FactorieVector]
      for (e <- n.edges) e.factor.setting.vect(e.index) = n.setting
    }
    val objective = factors.iterator.map(f => f.processor.gradientAndValue(f.setting, f.gradient)).sum
    val contState = contNodes.map(n => n.variable -> n.setting).toMap[Var[Any], Any]
    val vectState = vectNodes.map(n => n.variable -> n.setting).toMap[Var[Any], Any]
    ArgmaxResult(State(contState ++ vectState), objective)
  }

  class FactorType(val pot: Pot) extends Factor {
    var gradient = new Setting(pot.discVars.length, pot.contVars.length, pot.vectVars.length)
    lazy val setting: PartialSetting = createObservation()
  }

  def createFactor(pot: Pot) = new FactorType(pot)

  private var stochastic: Seq[FactorType] = Seq.empty

  class ResamplingTrainer(fg: GradientBasedOptimizer, self: Trainer) extends Trainer {
    def processExamples(examples: Iterable[Example]) = {
      self.processExamples(examples)
      for (f <- stochastic) reattachFactor(f)
    }
    def isConverged = self.isConverged
  }

}

class ExampleStochasticPot(v: => VectVar, w: => VectVar) extends StatelessDifferentiable with StatelessScorer with VectPotential {
  def vectVars = Array(v, w)
  def gradientAndValue(currentParameters: PartialSetting, gradient: Setting) = ???
  def score(setting: Setting) = ???
}

/**
 * Create an optimizer for the given problem.
 */
class GradientBasedArgmaxer(val sum: Sum[Differentiable],
                            trainer: WeightsSet => Trainer = w => new OnlineTrainer(w, new AdaGrad(), 100),
                            stochasticPotentials: Seq[Differentiable] = Seq.empty
                           ) extends EmptyEdgeFactorGraph
                                     with EmptyNodeFactorGraph
                                     with Argmaxer {

  type Pot = Differentiable
  type Processor = GradientCalculator
  def processor(pot: Pot) = pot.gradientCalculator()


  lazy val problem = Problem(sum.args)
  stochastic = stochasticPotentials.map(pot2Factor)

  val weightsKeys = new mutable.HashMap[Node, Weights]()
  val weightsSet  = new WeightsSet

  for (n <- contNodes) {weightsKeys(n) = weightsSet.newWeights(new DenseTensor1(Array(n.setting)))}
  for (n <- vectNodes) {weightsKeys(n) = weightsSet.newWeights(n.setting)}


  val learner = new ResamplingTrainer(this, trainer(weightsSet))

  val examples = for (f <- factors) yield new Example {
    val factor = f
    def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator) = {

      for (e <- f.contEdges; if !e.node.observed) {
        f.setting.cont(e.index) = weightsSet(weightsKeys(e.node)).asInstanceOf[FactorieVector](0)
      }
      for (e <- f.vectEdges; if !e.node.observed) {
        f.setting.vect(e.index) = weightsSet(weightsKeys(e.node)).asInstanceOf[FactorieVector]
      }
      val v = f.processor.gradientAndValue(f.setting, f.gradient)
      for (e <- f.contEdges; if !e.node.observed) {
        gradient.accumulate(weightsKeys(e.node), new DenseTensor1(Array(f.gradient.cont(e.index))), 1.0)
      }
      for (e <- f.vectEdges; if !e.node.observed) {
        gradient.accumulate(weightsKeys(e.node), f.gradient.vect(e.index), 1.0)
      }

      value.accumulate(v)
    }
  }

  def argmax(observed: PartialSetting, incoming: Msgs, result: Setting, score: DoubleBuffer) = {
    //set observations
    //todo: should clear observations from last time
    //todo: take care of incoming messages, may need to add incoming messages to gradient methods?

    for (i <- 0 until observed.disc.length; if observed.discObs(i)) {
      var2DiscNode(sum.discVars(i)).observe(observed.disc(i))
    }
    for (i <- 0 until observed.cont.length; if observed.contObs(i)) {
      var2ContNode(sum.contVars(i)).observe(observed.cont(i))
    }
    for (i <- 0 until observed.vect.length; if observed.vectObs(i)) {
      var2VectNode(sum.vectVars(i)).observe(observed.vect(i))
    }

    for ((f, argMap) <- factors zip sum.argMaps) {
      f.setting.copyFrom(observed, argMap)
      //observed.copyTo(f.setting, argMap)
    }

    learner.trainFromExamples(examples)

    //update node values
    for (n <- contNodes) {
      n.setting = weightsSet(weightsKeys(n)).asInstanceOf[FactorieVector](0)
      for (e <- n.edges) e.factor.setting.cont(e.index) = n.setting
    }
    for (n <- vectNodes) {
      n.setting = weightsSet(weightsKeys(n)).asInstanceOf[FactorieVector]
      for (e <- n.edges) e.factor.setting.vect(e.index) = n.setting
    }

    //copy to setting
    for (i <- 0 until observed.disc.length) {
      result.disc(i) = if (!observed.discObs(i)) var2DiscNode(sum.discVars(i)).setting else observed.disc(i)
    }
    for (i <- 0 until observed.cont.length) {
      result.cont(i) = if (!observed.contObs(i)) var2ContNode(sum.contVars(i)).setting else observed.cont(i)
    }
    for (i <- 0 until observed.vect.length) {
      result.vect(i) = if (!observed.vectObs(i)) var2VectNode(sum.vectVars(i)).setting else observed.vect(i)
    }

    val objective = factors.iterator.map(f => f.processor.gradientAndValue(f.setting, f.gradient)).sum

    score.value = objective
  }


  class FactorType(val pot: Pot) extends Factor {
    var gradient = new Setting(pot.discVars.length, pot.contVars.length, pot.vectVars.length)
    lazy val setting: PartialSetting = createObservation()
  }

  def createFactor(pot: Pot) = new FactorType(pot)

  private var stochastic: Seq[FactorType] = Seq.empty

  class ResamplingTrainer(fg: GradientBasedArgmaxer, self: Trainer) extends Trainer {
    def processExamples(examples: Iterable[Example]) = {
      self.processExamples(examples)
      for (f <- stochastic) reattachFactor(f)
    }
    def isConverged = self.isConverged
  }

}



