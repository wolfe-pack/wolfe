package ml.wolfe.benchmark

import ml.wolfe.{MaxProduct, Wolfe}
import ml.wolfe.macros.OptimizedOperators
import ml.wolfe.util.{Evaluator, LoggerUtil, NLP}
import cc.factorie.optimize.{Perceptron, OnlineTrainer}

/**
 * @author Sebastian Riedel
 */
object ChunkingBenchmark {

  def main(args: Array[String]) {

    import Wolfe._
    import OptimizedOperators._
    import NLP._

    def toToken(conll: Array[String]) = Token(conll(0), Tag(conll(1)), Chunk(conll(2)))

    val train = loadCoNLL("ml/wolfe/datasets/conll2000/train.txt")(toToken).map(Sentence).take(1000)

    def Tokens = Wolfe.all(Token)
    def Sentences = Wolfe.all(Sentence)(seqs(Tokens))

    def observed(s: Sentence) = s.copy(tokens = s.tokens.map(_.copy(chunk = hide[Chunk])))
    def evidence(s1: Sentence)(s2: Sentence) = observed(s1) == observed(s2)

    def features(s: Sentence) = {
      import s._
      val f1 = sum { over(0 until tokens.size) of (i => oneHot('o -> tokens(i).word -> tokens(i).chunk)) }
      val f2 = sum { over(0 until tokens.size - 1) of (i => oneHot('p -> tokens(i).chunk -> tokens(i + 1).chunk)) }
      f1 + f2
    }

    @OptimizeByInference(MaxProduct(_, 1))
    def model(w: Vector)(s: Sentence) = w dot features(s)

    def perceptronLoss(w: Vector)(s: Sentence) = max { over(Sentences) of model(w) st evidence(s) } - model(w)(s)

    @OptimizeByLearning(new OnlineTrainer(_, new Perceptron, 10, 100))
    def loss(w: Vector) = sum { over(train) of perceptronLoss(w) }

    val w = argmin { over[Vector] of loss }

    //the predictor given some observed instance
    def predict(s: Sentence) = argmax { over(Sentences) of model(w) st evidence(s) }

    //            val predictedTest = map { over(test) using predict }
    LoggerUtil.info("Prediction ...")
    val predictedTrain = map { over(train) using predict }

    //      println(predictedTrain.mkString("\n"))
    //
    val evalTrain = Evaluator.evaluate(train.flatMap(_.tokens), predictedTrain.flatMap(_.tokens))(_.chunk)

    println(evalTrain)
    //      val evalTest = Evaluator.evaluate(test, predictedTest)(_.irisClass)
    //
    //      evalTrain.f1 should be(0.93 +- 0.01)
    //      evalTest.f1 should be(0.98 +- 0.01)
  }

}
