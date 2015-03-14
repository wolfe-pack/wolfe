package ml.wolfe.term

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

/**
 * @author riedel
 */
class FG[NodeContent, EdgeContent, FactorContent] {

  class Node(val variable: AnyVar, val content: NodeContent) {
    val edges = new ArrayBuffer[Edge]
    val activeEdges = new ArrayBuffer[Edge]
  }

  class Edge(val node: Node, val factor: Factor, val content: EdgeContent)

  class Factor(val potential: DoubleTerm, val content: FactorContent) {
    val edges = new ArrayBuffer[Edge]
    def activeEdges = edges
  }


  val nodes = new mutable.LinkedHashMap[AnyVar, Node]
  val factors = new mutable.LinkedHashMap[DoubleTerm, Factor]
  val edges = new ArrayBuffer[Edge]()

  val activeFactors = new mutable.LinkedHashSet[Factor]
  val activeEdges = new mutable.LinkedHashSet[Edge]
  val activeNodes = new mutable.LinkedHashSet[Node]


  def addNode(variable: AnyVar, content: NodeContent): Node = {
    val node = new Node(variable, content)
    nodes(variable) = node
    node
  }


  def addFactor(potential: DoubleTerm, content: FactorContent)(edgeContent: Node => EdgeContent): Factor = {
    val factor = new Factor(potential, content)
    for (v <- potential.vars; n <- nodes.get(v)) {
      val edge = new Edge(n, factor, edgeContent(n))
      edges += edge
      factor.edges += edge
      n.edges += edge
    }
    factors(potential) = factor
    factor
  }


  def deactivate(): Unit = {
    for (n <- activeNodes) n.activeEdges.clear()
    activeNodes.clear()
    activeEdges.clear()
    activeFactors.clear()
  }

  private def deactivate(f: Factor): Unit = {
    for (e <- f.edges) {
      e.node.activeEdges -= e
      activeEdges -= e
      if (e.node.activeEdges.isEmpty) activeNodes -= e.node
    }
    activeFactors -= f
  }


  def activate(factor: Factor): Unit = {
    for (e <- factor.edges) {
      e.node.activeEdges += e
      activeEdges += e
      activeNodes += e.node
    }
    activeFactors += factor
  }

}
