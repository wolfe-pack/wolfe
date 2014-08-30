package ml.wolfe.fg

import ml.wolfe.FactorGraph.Edge
import ml.wolfe.{Wolfe, WolfeSpec}

/**
 * @author Sebastian Riedel
 */
class TreePotentialSpecs extends WolfeSpec {


  import TreePotential._

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
