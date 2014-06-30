package ml.wolfe

import scala.collection.mutable.ArrayBuffer
import scalaxy.loops._
import scala.annotation.tailrec
import scala.collection.mutable
import ml.wolfe.fg._


/**
 * A simple Factor Graph with infrastructure for message passing.
 */
final class FactorGraph {

  import FactorGraph._

  val edges   = new ArrayBuffer[Edge]
  val nodes   = new ArrayBuffer[Node]
  val factors = new ArrayBuffer[Factor]

  /**
   * Stochastic factors can be
   */
  val stochasticFactors = new ArrayBuffer[(Factor,()=>Seq[Node])]()

  /**
   * A flag that indicates whether the algorithm that uses this graph to find a solution has converged
   */
  var converged: Boolean = false

  /**
   * Linear factors can depend on a weight vector to calculate their scores.
   */
  var weights: FactorieVector = null

  /**
   * Algorithms that use message passing to calculate a value (maximum, log partition function ...) can store the
   * result here.
   */
  var value = 0.0

  /**
   * Algorithms that calculate gradients (or sub-gradients) can store their results here.
   */
  var gradient: FactorieVector = null

  /**
   * Adds a node for a variable of domain size `dim`
   * @param dim size of domain of corresponding variable
   * @return the added node.
   */
  def addNode(dim: Int) = {
    val n = new Node(nodes.size, dim)
    nodes += n
    n
  }

  /**
   * Adds an edge between node and factor
   * @param f factor to connect.
   * @param n node to connect.
   * @param indexInFactor the index the edge has in the factor.
   * @return the added edge.
   */
  def addEdge(f: Factor, n: Node, indexInFactor: Int): Edge = {
    val e = new Edge(n, f, n.variable.asDiscrete.dim)
    e.indexInFactor = indexInFactor
    n.edgeCount += 1
    f.edgeCount += 1
    edges += e
    e
  }

  /**
   * Adds an edge between node and factor
   * @param f factor to connect.
   * @param n node to connect.
   * @return the added edge.
   */
  def addEdge(f: Factor, n: Node): Edge = addEdge(f, n, f.edgeCount)

  /**
   * Creates a new factor, no potential assigned.
   * @return the created factor.
   */
  def addFactor() = {
    val f = new Factor(this, factors.size)
    factors += f
    f
  }

  /**
   * Adds a factor whose nodes will be resampled.
   * @param sampleNodes a function that samples neighbors of the factor
   */
  def addStochasticFactor(sampleNodes: =>Seq[Node]) {
    val f = addFactor()
    stochasticFactors += f -> (() =>sampleNodes)
  }

  /**
   * Change the arguments of each stochastic factor.
   */
  def sampleFactors() {
    for ((f,s) <- stochasticFactors) {
      moveFactor(f,s())
    }
  }

  /**
   * Removes the factor from the graph. Very expensive.
   * @param factor the factor to remove.
   */
  def removeFactor(factor: Factor) {
    for (e <- factor.edges) {
      e.n.edges = e.n.edges.filter(_ != e)
      e.n.edgeCount -= 1
      edges.remove(edges.indexOf(e))
    }
    factors.remove(factors.indexOf(factor))
  }

  /**
   * Method to efficiently "move" a factor from one set of nodes
   * to another. Factor will retain its potential and edges, only
   * the edge nodes are changed.
   * @param factor factor to move.
   * @param newNodes the new nodes the factor should point to. It should be the same number and types of
   *                 variables as the factor connected before.
   */
  def moveFactor(factor: Factor, newNodes: Seq[Node]) {
    require(newNodes.size == factor.edges.size)
    for ((e, n) <- factor.edges zip newNodes) {
      //todo: some of the array recreation code may be a bottleneck.
      e.n.edges = e.n.edges.filter(_ != e)
      e.n.edgeCount -= 1
      e.n = n
      e.n.edgeCount += 1
      e.n.edges = e.n.edges :+ e
    }
  }

  /**
   * creates node message buffers and domains if necessary.
   */
  def setupNodes() {
    for (node <- nodes) node.variable.setup()
  }

  /**
   * Connecting nodes and factors to the edges between them.
   */
  def build() {
    setupNodes()
    for (edge <- edges) {
      if (edge.f.edges.length != edge.f.edgeCount) edge.f.edges = Array.ofDim[Edge](edge.f.edgeCount)
      if (edge.n.edges.length != edge.n.edgeCount) edge.n.edges = Array.ofDim[Edge](edge.n.edgeCount)
      edge.indexInNode = edge.n.edgeFilled
      edge.f.edges(edge.indexInFactor) = edge
      edge.n.edges(edge.indexInNode) = edge
      edge.f.edgeFilled += 1
      edge.n.edgeFilled += 1
    }
  }


  /**
   * Verbose string representation of the graph.
   * @param fgPrinter a printer for nodes and factors.
   * @return string representation of graph.
   */
  def toVerboseString(fgPrinter: FGPrinter = DefaultPrinter) = {
    f"""
      |Nodes:
      |${ nodes.map(_.toVerboseString(fgPrinter.node2String)).mkString("\n") }
      |
      |Factors:
      |${ factors.map(_.toVerboseString(fgPrinter)).mkString("\n") }
      |
      |Edges:
      |${ edges.map(_.toVerboseString(fgPrinter)).mkString("\n") }
    """.stripMargin
  }


  def getNode(index: Int) = nodes(index)

  def getFactor(index: Int) = factors(index)


}

object FactorGraph {


  /**
   * A node representing a variable.
   * @param index the index of the node.
   * @param dim the dimension of the variable the node is representing.
   */
  final class Node(val index: Int, dim: Int) {
    /* all edges to factors that this node is connected to */
    var edges: Array[Edge] = Array.ofDim(0)

    var variable:Var = new DiscreteVar(dim)

    def toVerboseString(nodePrinter: Node => String = n => "") = {
      f"""-----------------
        |Node:   $index%3d ${ nodePrinter(this) }
        |Var:    $variable
      """.stripMargin
    }

    override def toString = "N" + index.toString

    private[FactorGraph] var edgeCount : Int = 0
    private[FactorGraph] var edgeFilled: Int = 0

    override def equals(other: Any): Boolean = other match {
      case that: Node =>
        index == that.index
      case _ => false
    }
    override def hashCode(): Int = {
      val state = Seq(index)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
  }

  /**
   * An edge between node and factor
   * @param n the node.
   * @param f the factor.
   * @param dim dimension of the node's variable.
   */
  final class Edge(var n: Node, val f: Factor, dim: Int) {
    var msgs:Msgs = new DiscreteMsgs(dim)

    var indexInFactor = -1
    var indexInNode   = -1

    def toVerboseString(fgPrinter: FGPrinter) =
      f"""----------
        |Edge
        |Node:    ${ n.index } ${ fgPrinter.node2String(n) }
        |Factor:  ${ f.index } ${ fgPrinter.factor2String(f) }
      """.stripMargin
    override def toString = s"${ f.index } -> ${ n.index }"
  }

  /**
   * A factor in a message passing factor graph.
   *
   * Some hints for the usage of Factors of FactorGraph
   * f.potential.table.settings    | f.potential.table.scores
   * 0 0 0         | 0.5
   * 0 0 1         | 0.8
   * 0 0 2         | 0.9
   * 0 1 0         | 1.0
   * 0 1 1         | 0.1
   * 0 1 2         | 0.2
   * print(index) = 3
   * print(dims)  = 1,2,3
   * Node 0:  factor.edges(0).n
   * Node 1:  factor.edges(1).n
   * Node 2:  factor.edges(2).n
   *
   * @param fg the factor graph.
   * @param index the index/id of the factor
   */
  final class Factor(val fg: FactorGraph, val index: Int) {
    var edges: Array[Edge] = Array.ofDim(0)

    def numNeighbors = edges.size

    /**
     * The potential for this factor. Usually created after edges to factor have been created as
     * the potential directly works with the edges.
     */
    var potential: Potential = null

    /**
     * More verbose string representation that shows that potential table depending on factor type.
     * @param fgPrinter a printer that can print nodes and factors.
     * @return A verbose string representation of this factor.
     */
    def toVerboseString(implicit fgPrinter: FGPrinter) = {
      val tableString = potential.toVerboseString
      f"""-----------------
        |Factor:  $index ${ fgPrinter.factor2String(this) }
        |Nodes:   ${ edges.map(_.n.index).mkString(" ") } ${ edges.map(e => fgPrinter.node2String(e.n)).mkString(" ") }
        |Table:
        |${ tableString }
      """.stripMargin
    }

    override def toString = index.toString

    private[FactorGraph] var edgeCount : Int = 0
    private[FactorGraph] var edgeFilled: Int = 0

    override def equals(other: Any): Boolean = other match {
      case that: Factor =>
        index == that.index
      case _ => false
    }
    override def hashCode(): Int = {
      val state = Seq(index)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
  }


  /**
   * A scheduler provides a canonical ordering of edges such that it resembles the message ordering of forward-backward.
   */
  trait MPScheduler {
    /**
     * @param node root node
     * @return correct message ordering for forward-backward pass
     */
    def schedule(node: Node): Seq[Edge]

    /**
     * Runs scheduler on all disconnected components of the graph
     * @param graph factor graph with (possibly) disconnected components
     * @return schedule for forward-backward over all disconnected components of the graph
     */
    def schedule(graph: FactorGraph): Seq[Edge] = {
      @tailrec
      def scheduleAcc(nodes: Seq[Node], done: Set[Node], acc: Seq[Edge]): Seq[Edge] = nodes match {
        case Nil => acc
        case head :: tail =>
          if (done.contains(head)) scheduleAcc(tail, done, acc)
          else {
            val edges = schedule(head)
            scheduleAcc(tail, done ++ edges.map(_.n), acc ++ edges)
          }
      }

      scheduleAcc(graph.nodes.toList, Set(), Seq())
    }

    def schedule(factor: Factor): Seq[Edge] = schedule(factor.edges.head)

    def schedule(edge: Edge): Seq[Edge] = schedule(edge.n)
  }

  object MPSchedulerImpl extends MPScheduler {
    object MPDirection extends Enumeration {
      val Forward, Backward = Value
    }

    /**
     * @param e edge whose node will become the root node
     * @param direction whether calculating forward or backward pass
     * @return correct ordering for messaging pass for given direction (excluding the staring edge)
     */
    def schedule(e: Edge, direction: MPDirection.Value, done: Set[Edge] = Set()): Seq[Edge] = {
      @tailrec
      def scheduleAcc(todo: Seq[Edge], done: Set[Edge], acc: Seq[Edge]): Seq[Edge] = todo match {
        case Nil => acc
        case head :: tail =>
          if (head.f.edgeCount == 1 || done.contains(head)) scheduleAcc(tail, done, acc)
          else {
            val siblings = head.f.edges.filterNot(todo.contains)
            val nephews = siblings.flatMap(sibling => sibling.n.edges.filterNot(_ == sibling))
            direction match {
              case MPDirection.Forward => scheduleAcc(tail ++ nephews, done + head, nephews ++ acc)
              case MPDirection.Backward => scheduleAcc(tail ++ nephews, done + head, acc ++ siblings)
            }
          }
      }
      scheduleAcc(Seq(e), done, Seq())
    }

    override def schedule(node: Node): Seq[Edge] = {
      @tailrec
      def forwardBackward(edges: Seq[Edge], done: Set[Edge], acc: Seq[Edge]): Seq[Edge] = edges.toList match {
        case Nil => acc
        case head :: tail =>
          val forward = schedule(head, MPDirection.Forward, done)
          val backward = schedule(head, MPDirection.Backward, done)
          val rest = tail.filterNot(e => forward.contains(e) || backward.contains(e))
          val middle = if (forward.contains(head) || backward.contains(head)) Nil else Seq(head)
          forwardBackward(rest, done ++ forward ++ backward, forward ++ middle ++ acc ++ backward)
      }

      forwardBackward(node.edges, Set(), Seq())
    }
  }

  /**
   * Printer of nodes, factors and edges. Helps debugging (as the graph itself is
   * independent of the original model it comes from.
   */
  trait FGPrinter {
    def node2String(node: Node): String
    def factor2String(factor: Factor): String
    def vector2String(vector: FactorieVector): String
  }


  object DefaultPrinter extends FGPrinter {
    def node2String(node: Node) = ""
    def factor2String(factor: Factor) = ""
    def vector2String(vector: ml.wolfe.FactorieVector) = vector.toString()
  }

}

