package ml.wolfe.macros

import org.scalatest.{Matchers, FreeSpec}
import ml.wolfe.{MPGraph, MaxProduct, Wolfe}
import ml.wolfe.macros.OptimizedOperators._
import ml.wolfe.macros.Library._
import cc.factorie.optimize.{AveragedPerceptron, OnlineTrainer}
import ml.wolfe.util.NLP.Sentence
import ml.wolfe.util.NLP.Tag
import ml.wolfe.util.NLP.Chunk
import ml.wolfe.util.NLP.Token
import ml.wolfe.MPGraph.SimpleScheduler


/**
 * Created by rockt on 09/04/2014.
 */
class NodeSchedulerSpec extends FreeSpec with Matchers {
  "A scheduler" - {
    "gives a valid edge ordering on a MPGraph" in {
      val graph = new MPGraph()
      (0 to 5).foreach(i => graph.addNode(0))
      (0 to 3).foreach(i => graph.addTableFactor(Array(), Array(), Array()))
      val edges = Array(
        0 -> 2,
        0 -> 0,
        0 -> 1,
        1 -> 3,
        1 -> 4,
        1 -> 2,
        2 -> 4,
        2 -> 5,
        3 -> 5
      )
      edges.foreach(edge => graph.addEdge(graph.getFactor(edge._1), graph.getNode(edge._2), 0))

      val actual = SimpleScheduler.schedule(graph).map(e => (e.f.index, e.n.index))
      val expected = List(0 -> 0, 0 -> 1, 1 -> 3, 3 -> 5, 0 -> 2, 2 -> 5, 1 -> 2, 2 -> 4, 1 -> 4)

      actual should be(expected)
    }

    "gives a valid edge ordering when adding features" ignore {
      import Wolfe._
      implicit val defaultChunks = Seq("?").map(Chunk)
      implicit val labels = Seq("O", "B", "I").map(Tag)
      def toFeatureVector(token: Token): Wolfe.Vector = {
        oneHot('word -> token.word -> token.tag) +
        oneHot('firstCap -> token.tag, I(token.word.head.isUpper))
      }
      def Sentences = Wolfe.all(Sentence)(seqs(Wolfe.all(Token)))
      def observed(s: Sentence) = s.copy(tokens = s.tokens.map(_.copy(tag = hidden)))
      def features(s: Sentence): Wolfe.Vector = {
        sum { over(0 until s.tokens.size) of (i => toFeatureVector(s.tokens(i))) } +
        sum { over(0 until s.tokens.size - 1) of (i => oneHot('transition -> s.tokens(i).tag -> s.tokens(i + 1).tag)) }
      }

      @OptimizeByInference(MaxProduct(_, 1))
      def model(w: Vector)(s: Sentence) = w dot features(s)
      def predictor(w: Vector)(s: Sentence) = argmax { over(Sentences) of model(w) st evidence(observed)(s) }

      @OptimizeByLearning(new OnlineTrainer(_, new AveragedPerceptron, 3, 1))
      def loss(data: Iterable[Sentence])(w: Vector) = sum { over(data) of (s => model(w)(predictor(w)(s)) - model(w)(s)) }
      def learn(data:Iterable[Sentence]) = argmin { over[Vector] of loss(data) }

      val train = Seq(
        Sentence(Seq(
          Token("This", "O", "?"),
          Token("is", "B", "?"),
          Token("a", "I", "?"),
          Token("test", "O", "?"),
          Token(".", "O", "?")
        )),
        Sentence(Seq(
          Token("This", "O", "?"),
          Token("is", "B", "?"),
          Token("another", "O", "?"),
          Token(".", "O", "?")
        ))
      )

      learn(train)
    }
  }
}
