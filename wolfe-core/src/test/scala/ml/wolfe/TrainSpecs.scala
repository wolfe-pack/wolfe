package ml.wolfe

import cc.factorie.optimize.{Perceptron, OnlineTrainer}
import ml.wolfe.macros.{Library, OptimizedOperators}
import ml.wolfe.util.NLP

/**
 * Created by luke on 13/06/14.
 */

class TrainSpecs extends WolfeSpec {

  import Wolfe._
  import NLP._
  import Library._
  import OptimizedOperators._

  "A Foo" should {
    "bar" in {
      def tokens = Wolfe.all(Token)(strings x Seq(Tag('DT), Tag('NN)) x Seq(Chunk('?)))
      def sentences = Wolfe.all(Sentence)(seqs(tokens))

      def obs(s: Sentence) = s.copy(tokens = s.tokens.map(_.copy(tag = hidden)))

      def features(s: Sentence) = sum(0 until s.tokens.size) { i =>
        oneHot('word -> s.tokens(i).word) outer oneHot('tag -> s.tokens(i).tag)
      }

      @OptimizeByInference(MaxProduct(_, 1))
      def model(w: Vector)(s: Sentence) = w dot features(s)
      def predictor(w: Vector)(s: Sentence) = argmax(sentences filter evidence(obs)(s)) { model(w) }

      @OptimizeByLearning(new OnlineTrainer(_, new Perceptron, 3, 100))
      def loss(data: Iterable[Sentence])(w: Vector) = sum(data) {
        s => model(w)(predictor(w)(s)) - model(w)(s)
      }
      def learn(data: Iterable[Sentence]) = argmin(vectors) { loss(data) }


      //---------------

      def trainSentence = Sentence(Seq(
        Token("the", 'DT, '?),
        Token("dog", 'NN, '?)
      ))

      val w = learn(Seq(trainSentence))
      println(predictor(w)(trainSentence))

    }
  }


}
