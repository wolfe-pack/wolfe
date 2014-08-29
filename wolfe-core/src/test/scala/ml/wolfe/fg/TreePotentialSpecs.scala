package ml.wolfe.fg

import ml.wolfe.FactorGraph.Edge
import ml.wolfe.{Wolfe, WolfeSpec}

/**
 * @author Sebastian Riedel
 */
class TreePotentialSpecs extends WolfeSpec {

  def isLoopy[T](graph: Map[(T, T), Boolean]) = {
    val edges = (graph filter (_._2) map (_._1)).toList
    val nodes = (graph.keys map (_._1)).toList.distinct
    val children = edges groupBy (_._1) mapValues(_ map (_._2))
    val parents = edges groupBy (_._2) mapValues(_ map (_._2))

    //def recurse(node: T, tree: Set[T], currentChain: Set[T]): Set[T] =

    false
  }


  def graph[T](pairs: (T, T)*) = (pairs map (x => x -> true)).toMap withDefaultValue false

  "A TreePotential" should {
    "Blah blah blah" in {

      @Wolfe.Potential(new TreePotential(_:Map[(Any,Any),Edge],true))
      def tree[T](tree: Map[(T, T), Boolean]) =
        if (isLoopy(tree)) Double.NegativeInfinity else 0.0

      val g = graph(0 -> 1, 1 -> 2, 2 -> 0)




    }
  }

}
