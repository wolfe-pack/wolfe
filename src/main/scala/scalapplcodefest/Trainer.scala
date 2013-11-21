package scalapplcodefest

import cc.factorie.optimize.{Perceptron, OnlineTrainer, Example}
import cc.factorie.util.DoubleAccumulator
import cc.factorie.la.WeightsMapAccumulator
import cc.factorie.WeightsSet

/**
 * @author Sebastian Riedel
 */
object Trainer {

  trait Inference {
    def state:State
    def obj:Double
    def feats:Vector
    def updateResult(newWeights:DenseVector)
  }


  object Inference {
    def maxProduct(maxIterations:Int)(model:LinearModel)(weights:DenseVector)(instance:State):Inference = ???
  }

  def train(model: LinearModel, instances: Seq[State]): Vector = {
    import TermImplicits._
    val weightsSet = new WeightsSet
    val key = weightsSet.newWeights( new DenseVector(10000))

    case class PerceptronExample(instance:State) extends Example {

      val target = instance.target
      val targetFeats = model.features.eval(target).right.get
      val conditioned = model | instance

      val distConds = TermConverter.distConds(conditioned)
      val distDots = TermConverter.distDots(distConds)
      val unrolled = TermConverter.unrollLambdas(distDots)
      val flatten = TermConverter.flattenDouble(unrolled)

      println(conditioned)
      println(distConds)
      println(distDots)
      println(unrolled)
      println(flatten)

      val aligned = MessagePassingGraphBuilder.build(flatten,model.weights)

      def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator) = {

        aligned.graph.weights = weightsSet(key).asInstanceOf[DenseVector]
        MaxProduct.run(aligned.graph, 1)

        val guessFeats = new SparseVector(100)
        val guessScore = MaxProduct.featureExpectationsAndObjective(aligned.graph, guessFeats)
        val guess = aligned.beliefToState()
        val targetScore = model.eval(target + SingletonState(model.weights,weightsSet(key))).right.get

        println("----------")
        println(s"Gold: $target")
        println(s"Gold Vector: \n ${ChunkingExample.key.vectorToString(targetFeats)}")
        println(s"Guess: $guess")
        println(s"Guess Vector: \n ${ChunkingExample.key.vectorToString(guessFeats)}")

        value.accumulate(guessScore - targetScore)
        //WARNING: side effect, guessfeats is changed
        gradient.accumulate(key,guessFeats,-1.0)
        gradient.accumulate(key,targetFeats)


      }
    }
    val examples = instances.map(PerceptronExample)
    val trainer = new OnlineTrainer(weightsSet,new Perceptron)
    trainer.trainFromExamples(examples)

    weightsSet(key).asInstanceOf[Vector]

  }


}
