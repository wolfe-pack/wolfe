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
