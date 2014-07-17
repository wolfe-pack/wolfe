package ml.wolfe.macros

import ml.wolfe.{Wolfe, FactorGraph}
import ml.wolfe.util.NLP._
import OptimizedOperators._
import Wolfe._
import ml.wolfe.FactorGraph.DefaultPrinter


class FactorGraphSpec extends StructureIsomorphisms {
  "A message passing graph" should {
    "assigns meaningful labels to its nodes and factors" in  {
      def space = Wolfe.all(Sentence)(seqsOfLength(5, Wolfe.all(Token)(Seq("blub") x Seq(Tag('blah)) x Seq(Chunk('blub)))))
      def features(s: Sentence) = {
         //       sum(0 until s.tokens.size) (i => oneHot('word -> s.tokens(i).word -> s.tokens(i).tag))
        sum(0 until s.tokens.size) (i => oneHot('word -> s.tokens(i).word -> s.tokens(i).chunk))
      }

      val w = oneHot(1)
      def potential(s: Sentence) = w dot features(s)

      val graph: FactorGraph = MetaStructuredFactor.structuredFactor[Sentence](space, potential).graph

      println(graph.nodes)
      graph.nodes.foreach(n => {
        println()
        println(n.toVerboseString())
      })

      println(graph.factors)
      graph.factors.foreach(f => {
        println()
        println(f.toVerboseString(DefaultPrinter))
      })
    }
  }
}
