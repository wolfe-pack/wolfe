package scalapplcodefest

import cc.factorie.optimize.{Perceptron, OnlineTrainer, Example}
import cc.factorie.util.DoubleAccumulator
import cc.factorie.la.WeightsMapAccumulator
import cc.factorie.WeightsSet

/**
 * Training algorithms.
 *
 * @author Sebastian Riedel
 */
object Trainer {

  import TermImplicits._

  /**
   * Trains a linear model using the provided inference algorithm in the inner loop of training.
   * @param model the model to train. It must be a term that can be converted to a linear model.
   * @param instances instances to train from.
   * @param maxIterations maximum number of training iterations through the corpus.
   * @param inferencer a function that takes conditioned models and returns an inference result.
   * @return learned weights.
   */
  def train(model: Term[Double], instances: Seq[State], maxIterations: Int,
            inferencer: Term[Double] => Inference = Inference.maxProductArgmax(1)): DenseVector = {
    val weightsSet = new WeightsSet
    val key = weightsSet.newWeights(new DenseVector(10000))
    val Linear(features,weights,_) = model

    case class PerceptronExample(instance: State) extends Example {

      val target = instance.target
      val targetFeats = features.eval(target).get

      def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator) = {

        val conditioned = model | instance | weights -> weightsSet(key).asInstanceOf[DenseVector]
        val inference = inferencer(conditioned)

        val guessFeats = inference.feats()
        val guessScore = inference.obj()
        val targetScore = model.eval(target + SingletonState(weights, weightsSet(key))).get

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
