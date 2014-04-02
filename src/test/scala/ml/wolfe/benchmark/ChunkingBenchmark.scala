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

    def Sentences = Wolfe.all(Sentence)(seqs(all(Token)))

    def observed(s: Sentence) = s.copy(tokens = s.tokens.map(_.copy(chunk = hide[Chunk])))
    def evidence(s1: Sentence)(s2: Sentence) = observed(s1) == observed(s2)

    def features(s: Sentence) = {
      sum { over(0 until s.tokens.size) of (i => oneHot('o -> s.tokens(i).word -> s.tokens(i).chunk)) } +
      sum { over(0 until s.tokens.size - 1) of (i => oneHot('p -> s.tokens(i).chunk -> s.tokens(i + 1).chunk)) }
    }

    @OptimizeByInference(MaxProduct(_, 1))
    def model(w: Vector)(s: Sentence) = w dot features(s)
    def predictor(w: Vector)(s: Sentence) = argmax { over(Sentences) of model(w) st evidence(s) }

    @OptimizeByLearning(new OnlineTrainer(_, new Perceptron, 10, 100))
    def loss(data: Iterable[Sentence])(w: Vector) = sum { over(data) of (s => model(w)(predictor(w)(s)) - model(w)(s)) } ////
    def learn(data:Iterable[Sentence]) = argmin { over[Vector] of loss(data) }

    val w = learn(train)
    val predictedTrain = map { over(train) using predictor(w) }
    val evalTrain = Evaluator.evaluate(train.flatMap(_.tokens), predictedTrain.flatMap(_.tokens))(_.chunk)
    println(evalTrain)
  }

}
