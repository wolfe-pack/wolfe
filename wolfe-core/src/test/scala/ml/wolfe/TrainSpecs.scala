package ml.wolfe

import cc.factorie.optimize.{Perceptron, OnlineTrainer}
import ml.wolfe.macros.{Library, OptimizedOperators}
import ml.wolfe.D3Implicits._

class TrainSpecs extends WolfeSpec {

  import Wolfe._
  import Library._
  import OptimizedOperators._

  "A small, separable linear chain" should {

    case class Token(word:String, tag:Symbol)
    def tags = Seq('DT, 'NN, 'MD, 'VB, 'IN)
    def tokens = Wolfe.all(Token)(strings x tags)

    case class Sentence(tokens:Seq[Token])
    def obs(s: Sentence) = s.copy(tokens = s.tokens.map(_.copy(tag = 'hidden)))
    def sentences = Wolfe.all(Sentence)(seqs(tokens))

    def features(s: Sentence) =
      sum(0 until s.tokens.size) { i =>
        oneHot(s.tokens(i).word -> s.tokens(i).tag)
      } + sum(0 until s.tokens.size-1) { i =>
        oneHot(s.tokens(i).tag -> s.tokens(i+1).tag)
      }

    def trainSentences = Seq(
      Sentence(Seq(  Token("the", 'DT), Token("fish", 'NN)  )),
      Sentence(Seq(  Token("the", 'DT), Token("can", 'NN))),
      Sentence(Seq(  Token("the", 'DT), Token("cat", 'NN), Token("can", 'MD), Token("fish", 'VB) )),
      Sentence(Seq(  Token("fish", 'NN), Token("can", 'MD), Token("fish", 'VB) ))
    )




    "be perfectly classified using Belief Propagation" in {
      @OptimizeByInference(BeliefPropagation(_, 1))
      def model(w: Vector)(s: Sentence) = w dot features(s)

      // -------------------------------------------------------------------------------------------
      def predictor(w: Vector)(s: Sentence) = argmax(sentences filter evidence(obs)(s)) { model(w) }
      @OptimizeByLearning(new OnlineTrainer(_, new Perceptron, 3, -1))
      def loss(data: Iterable[Sentence])(w: Vector) = sum(data) {
        s => model(w)(predictor(w)(s)) - model(w)(s)
      }
      def learn(data: Iterable[Sentence]) = argmin(vectors) { loss(data) }
      val w = learn(trainSentences)
      trainSentences.map(predictor(w)) shouldEqual trainSentences
    }


    "be perfectly classified using Dual Decomposition" in {
      @OptimizeByInference(DualDecomposition(_, 10, 1))
      def model(w: Vector)(s: Sentence) = w dot features(s)

      // -------------------------------------------------------------------------------------------
      def predictor(w: Vector)(s: Sentence) = argmax(sentences filter evidence(obs)(s)) { model(w) }
      @OptimizeByLearning(new OnlineTrainer(_, new Perceptron, 3, -1))
      def loss(data: Iterable[Sentence])(w: Vector) = sum(data) {
        s => model(w)(predictor(w)(s)) - model(w)(s)
      }
      def learn(data: Iterable[Sentence]) = argmin(vectors) { loss(data) }
      val w = learn(trainSentences)
      trainSentences.map(predictor(w)) shouldEqual trainSentences
    }


    "be perfectly classified using AD3" in {
      @OptimizeByInference(DualDecomposition.ad3(_, 10))
      def model(w: Vector)(s: Sentence) = w dot features(s)

      // -------------------------------------------------------------------------------------------
      def predictor(w: Vector)(s: Sentence) = argmax(sentences filter evidence(obs)(s)) { model(w) }
      @OptimizeByLearning(new OnlineTrainer(_, new Perceptron, 3, 100))
      def loss(data: Iterable[Sentence])(w: Vector) = sum(data) {
        s => model(w)(predictor(w)(s)) - model(w)(s)
      }
      def learn(data: Iterable[Sentence]) = argmin(vectors) { loss(data) }
      val w = learn(trainSentences)
      trainSentences.map(predictor(w)) shouldEqual trainSentences
    }
  }


}
