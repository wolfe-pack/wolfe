package ml.wolfe.macros

import ml.wolfe.Wolfe
import ml.wolfe.util.NLP
import Wolfe._
import NLP._
import OptimizedOperators._


/**
 * Created by rockt on 09/04/2014.
 */
class NodeSchedulerSpec extends StructureIsomorphisms {
  /*
  "A scheduler" should {
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
  }
  */

  "A MetaStructuredFactor" should {
    "does not generate cycles" in {
      def space = Wolfe.all(Sentence)(seqs(5, Wolfe.all(Token)(Seq("blub") x Seq(Tag("blah")) x Seq(Chunk("blub")))))

      def features(s: Sentence) = {
//        sum { over(0 until s.tokens.size) of (i => oneHot('word -> s.tokens(i).word -> s.tokens(i).tag)) }
        sum { over(0 until s.tokens.size) of (i => oneHot('word -> s.tokens(i).word -> s.tokens(i).chunk)) }
      }

      val w = oneHot(1)
      def potential(s: Sentence) = w dot features(s)

      val factor = MetaStructuredFactor.structuredFactor[Sentence](space, potential)

      //TODO test for cycles
      println(factor.graph.edges)
    }
  }
}
