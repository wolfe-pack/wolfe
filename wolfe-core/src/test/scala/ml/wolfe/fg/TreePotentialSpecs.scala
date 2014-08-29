package ml.wolfe.fg

import ml.wolfe.FactorGraph.Edge
import ml.wolfe.{Wolfe, WolfeSpec}

/**
 * @author Sebastian Riedel
 */
class TreePotentialSpecs extends WolfeSpec {

  def isFullyConnectedTree[T](graph: Map[(T, T), Boolean]) = {
    val edges = (graph filter (_._2) map (_._1)).toList
    val nodes = (graph.keys map (_._1)).toList.distinct
    val children = edges groupBy (_._1) mapValues (_ map (_._2))

    //def recurse(node: T, tree: Set[T], currentChain: Set[T]): Set[T] =
    def reached(remainder: List[T], covered: Set[T] = Set.empty): Set[T] = remainder match {
      case Nil => covered
      case h :: t if covered(h) => reached(t, covered)
      case h :: t =>
        val toVisit = children.getOrElse(h, Nil)
        reached(toVisit, covered ++ toVisit)
    }
    val result = reached(nodes)
    result.size == nodes.size - 1
  }


  def graph[T](pairs: (T, T)*) = (pairs map (x => x -> true)).toMap withDefaultValue false

  "A tree constraint checker" should {
    "find a loop" in {
      isFullyConnectedTree(graph(0 -> 1, 1 -> 2, 2 -> 0)) should be(false)
    }

    "find a legal tree" in {
      isFullyConnectedTree(graph(0 -> 1, 1 -> 2)) should be(true)
    }
  }

  "A TreePotential" should {
    "Blah blah blah" in {

      @Wolfe.Potential(new TreePotential(_: Map[(Any, Any), Edge], true))
      def tree[T](tree: Map[(T, T), Boolean]) =
        if (isFullyConnectedTree(tree)) 0.0 else Double.NegativeInfinity

      val g = graph(0 -> 1, 1 -> 2, 2 -> 0)

      println(isFullyConnectedTree(graph(0 -> 1, 1 -> 2, 2 -> 0)))
      println(isFullyConnectedTree(graph(0 -> 1, 1 -> 2, 2 -> 0)))


    }
  }

}
