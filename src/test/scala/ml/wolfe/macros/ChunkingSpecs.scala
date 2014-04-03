package ml.wolfe.macros

import ml.wolfe.{MaxProduct, Wolfe, WolfeSpec}
import cc.factorie.optimize.{Perceptron, OnlineTrainer}
import ml.wolfe.util.{LoggerUtil, NLP, Evaluator}

/**
 * @author Sebastian Riedel
 */
class ChunkingSpecs extends WolfeSpec {

  "A Chunking Model" should {
    "give reasonable performance on the CoNLL dataset" in {


      import Wolfe._
      import OptimizedOperators._
      import NLP._

      def toToken(conll: Array[String]) = Token(conll(0), Tag(conll(1)), Chunk(conll(2)))

      val train = loadCoNLL("ml/wolfe/datasets/conll2000/train.txt")(toToken).map(Sentence).take(100)

      def Tokens = Wolfe.all(Token)
      def Sentences = Wolfe.all(Sentence)(seqs(Tokens))

      def observed(s: Sentence) = s.copy(tokens = s.tokens.map(_.copy(chunk = hide[Chunk])))
      def evidence(s1: Sentence)(s2: Sentence) = observed(s1) == observed(s2)

      def features(s: Sentence) = {
        import s._
        val obs = sum { over(0 until tokens.size) of (i => oneHot('o -> tokens(i).word -> tokens(i).chunk)) }
        val pairs = sum { over(0 until tokens.size - 1) of (i => oneHot('p -> tokens(i).chunk -> tokens(i + 1).chunk)) }
        obs + pairs
      }

      @OptimizeByInference(MaxProduct(_, 1))
      def model(w: Vector)(s: Sentence) = w dot features(s)

      def perceptronLoss(w: Vector)(i: Sentence): Double = max { over(Sentences) of model(w) st evidence(i) } - model(w)(i)

      @OptimizeByLearning(new OnlineTrainer(_, new Perceptron, 4))
      def loss(w: Vector) = sum { over(train) of perceptronLoss(w) }

      val w = argmin { over[Vector] of loss }

      //the predictor given some observed instance
      def predict(s: Sentence) = argmax { over(Sentences) of model(w) st evidence(s) }

      val predictedTrain = map { over(train) using predict }
      val evalTrain = Evaluator.evaluate(train.flatMap(_.tokens), predictedTrain.flatMap(_.tokens))(_.chunk)
      evalTrain.f1 should be(0.88 +- 0.01)

      //      val evalTest = Evaluator.evaluate(test, predictedTest)(_.irisClass)
      //
      //      evalTrain.f1 should be(0.93 +- 0.01)
      //      evalTest.f1 should be(0.98 +- 0.01)


    }


  }

}
