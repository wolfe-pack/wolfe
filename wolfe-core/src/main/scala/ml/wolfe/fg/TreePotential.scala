package ml.wolfe.fg

import ml.wolfe.FactorGraph.Edge

/**
 * @author Sebastian Riedel
 */
class TreePotential(edges:Map[(Any,Any),Edge], multinode:Boolean) extends Potential {



  /**
   * Calculate and update the MAP messages from the factor of this potential to all edges.
   */
  override def mapF2N() = {
    val in = edges(1 -> 2).msgs.asDiscrete
    val factor2nodeMsg = in.f2n
    val node2factorMsg = in.n2f

    val trueScoreFromNode = node2factorMsg(1)

  }
  override def valueForCurrentSetting() = {
    val graph = edges mapValues(_.n.variable.asDiscrete.value == 1)
    val tree = TreePotential.isFullyConnectedTree(graph)
    if (tree) 0.0 else Double.NegativeInfinity
  }
}

object TreePotential {

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

}
