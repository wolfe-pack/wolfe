package scalapplcodefest

import cc.factorie.optimize.{Perceptron, OnlineTrainer, Example}
import cc.factorie.util.DoubleAccumulator
import cc.factorie.la.WeightsMapAccumulator
import cc.factorie.WeightsSet

/**
 * @author Sebastian Riedel
 */
object Trainer {


  def train(model: LinearModel, instances: Seq[State],
            maxIterations:Int,
            inferencer:Inferencer = Inference.maxProduct(1)): DenseVector = {
    val weightsSet = new WeightsSet
    val key = weightsSet.newWeights(new DenseVector(10000))

    case class PerceptronExample(instance: State) extends Example {

      val target = instance.target
      val targetFeats = model.features.eval(target).right.get
      val inference = inferencer(model)(weightsSet(key).asInstanceOf[DenseVector])(instance)

      def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator) = {

        inference.updateResult(weightsSet(key).asInstanceOf[DenseVector])

        val guessFeats = inference.feats()
        val guessScore = inference.obj()
        val guess = inference.state()
        val targetScore = model.eval(target + SingletonState(model.weights, weightsSet(key))).right.get

//        println("----------")
//        println(s"Gold: $target")
//        println(s"Gold Vector: \n ${ChunkingExample.key.vectorToString(targetFeats)}")
//        println(s"Guess: $guess")
//        println(s"Guess Vector: \n ${ChunkingExample.key.vectorToString(guessFeats)}")

        value.accumulate(guessScore - targetScore)
        //todo: WARNING: side effect, guessfeats is changed
        gradient.accumulate(key, guessFeats, -1.0)
        gradient.accumulate(key, targetFeats)


      }
    }
    val examples = instances.map(PerceptronExample)
    val trainer = new OnlineTrainer(weightsSet, new Perceptron, maxIterations)
    trainer.trainFromExamples(examples)

    weightsSet(key).asInstanceOf[DenseVector]

  }


}

trait Inference {
  def state(): State
  def obj(): Double
  def feats(): SparseVector
  def updateResult(newWeights: DenseVector)
}


object Inference {
  def maxProduct(maxIterations: Int)(model: LinearModel)(weights: DenseVector)(instance: State): Inference = {
    import TermImplicits._

    //unrolling
    val conditioned = model | instance
    val flattenQuantified = TermConverter.flatten(conditioned, Math.VecAdd)
    val distConds = TermConverter.distConds(flattenQuantified)
    val groupedLambdas = TermConverter.groupLambdasDeep(distConds)
    val distDots = TermConverter.distDots(groupedLambdas)
    val unrolled = TermConverter.unrollLambdas(distDots)
    val flatten = TermConverter.flatten(unrolled, Math.DoubleAdd)

    println(conditioned)
    println(flattenQuantified)
    println(distConds)
    println(groupedLambdas)
    println(distDots)
    println(unrolled)
    println(flatten)

    val aligned = MessagePassingGraphBuilder.build(flatten, model.weights)

    val result = new Inference {
      private var dirtyState = true
      private var dirtyObjAndFeats = true
      private var _state:State = null
      private var _obj:Double = Double.NegativeInfinity
      private var _feats:SparseVector = null
      private def inferState() {
        if (dirtyState) {
          MaxProduct.run(aligned.graph, maxIterations)
          dirtyState = false
        }
      }
      private def inferObjAndFeatures() {
        if (dirtyObjAndFeats) {
          inferState()
          _feats = new SparseVector(10)
          _obj = MaxProduct.featureExpectationsAndObjective(aligned.graph,_feats)
          dirtyObjAndFeats = false
        }
      }

      def state() = {inferState(); aligned.argmaxState()}
      def obj() = {inferObjAndFeatures(); _obj}
      def feats() = {inferObjAndFeatures(); _feats }
      def updateResult(newWeights: DenseVector) = {
        aligned.graph.weights = weights
        println(aligned.graph.toVerboseString(new aligned.FGPrinter(ChunkingExample.key)))
        dirtyState = true
        dirtyObjAndFeats = true
      }
    }
    result.updateResult(weights)
    result
  }
}