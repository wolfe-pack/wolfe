package ml.wolfe.examples

import ml.wolfe.{BeliefPropagation, Wolfe}
import ml.wolfe.macros.{Library, OptimizedOperators}
import ml.wolfe.util.{Evaluator, NLP}
import cc.factorie.optimize.{Perceptron, OnlineTrainer}

/**
 * @author Sebastian Riedel
 */
object ChunkingExample {

  import Wolfe._
  import OptimizedOperators._
  import NLP._
  import Library._

  def Sentences = Wolfe.all(Sentence)(seqs(all(Token)))

  def observed(s: Sentence) = s.copy(tokens = s.tokens.map(_.copy(chunk = hidden)))

  def features(s: Sentence) = {
    sum(0 until s.tokens.size) { i => oneHot('o -> s.tokens(i).word -> s.tokens(i).chunk) } +
    sum(0 until s.tokens.size - 1) { i => oneHot('p -> s.tokens(i).chunk -> s.tokens(i + 1).chunk) }
  }

  @OptimizeByInference(BeliefPropagation(_, 1))
  def model(w: Vector)(s: Sentence) = w dot features(s)
  def predictor(w: Vector)(s: Sentence) = argmax(Sentences filter evidence(observed)(s)) { model(w) }

  @OptimizeByLearning(new OnlineTrainer(_, new Perceptron, 3, 100))
  def loss(data: Iterable[Sentence])(w: Vector) = sum(data) { s => model(w)(predictor(w)(s)) - model(w)(s) }
  ////
  def learn(data: Iterable[Sentence]) = argmin(vectors) { loss(data) }

  def main(args: Array[String]) {

    def toToken(conll: Array[String]) = Token(conll(0), Tag(Symbol(conll(1))), Chunk(Symbol(conll(2))))

    val train = loadCoNLL("ml/wolfe/datasets/conll2000/train.txt")(toToken).map(Sentence).take(1000)
    val w = learn(train)
    val predictedTrain = map(train) { predictor(w) }
    val evalTrain = Evaluator.evaluate(train.flatMap(_.tokens), predictedTrain.flatMap(_.tokens))(_.chunk)

    println(evalTrain)
  }

}
