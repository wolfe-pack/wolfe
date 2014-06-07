package ml.wolfe

import scala.collection.mutable.ArrayBuffer
import scalaxy.loops._
import scala.annotation.tailrec
import scala.collection.mutable
import ml.wolfe.potential.Potential


/**
 * A simple Factor Graph with infrastructure for message passing.
 * It stores messages and table potentials in double arrays.
 * Factors can have one of three types:
 * (1) simple dense table based;
 * (2) based on a feature vector for each state that is multiplied with a shared weight vector
 * (3) generic (calls a function to be evaluated at a particular state)
 *
 *
 */
final class FactorGraph {

  import FactorGraph.FactorType._
  import FactorGraph._

  val edges   = new ArrayBuffer[Edge]
  val nodes   = new ArrayBuffer[Node]
  val factors = new ArrayBuffer[Factor]


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
    val e = new Edge(n, f, n.dim)
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
   * creates node message buffers and domains if necessary.
   */
  def setupNodes() {
    for (node <- nodes) {
      if (node.domain == null || node.domain.length != node.dim) node.domain = Array.range(0, node.dim)
      if (node.b == null || node.b.length != node.dim) node.b = Array.ofDim[Double](node.dim)
      if (node.in == null || node.in.length != node.dim) node.in = Array.ofDim[Double](node.dim)
    }
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
   * We define a set of factor types. Inference algorithms will use the type of a factor
   * to determine how to treat the factor. We use a type instead of sub-classing to
   * avoid any kind of polymorphism inside the inner loop of inference.
   */
  object FactorType extends Enumeration {
    val TABLE, LINEAR, GENERIC = Value
  }

  import FactorType._


  /**
   * Turns a setting vector into an entry number.
   * @param setting setting
   * @param dims dimensions of each variable.
   * @return the entry corresponding to the given setting.
   */
  final def settingToEntry(setting: Array[Int], dims: Array[Int]) = {
    var result = 0
    for (i <- (0 until dims.length).optimized) {
      result = setting(i) + result * dims(i)
    }
    result
  }

  /**
   * Turns an entry into a setting
   * @param entry the entry number.
   * @param dims dimensions of the variables.
   * @return a setting array corresponding to the entry.
   */
  final def entryToSetting(entry: Int, dims: Array[Int]) = {
    val result = Array.ofDim[Int](dims.length)
    var current = entry
    for (i <- (0 until dims.length).optimized) {
      val value = current % dims(i)
      result(i) = value
      current = current / dims(i)
    }
    result
  }

  /**
   * A node representing a variable.
   * @param index the index of the node.
   * @param dim the dimension of the variable the node is representing.
   */
  final class Node(val index: Int, var dim: Int) {
    /* all edges to factors that this node is connected to */
    var edges: Array[Edge] = Array.ofDim(0)

    /* node belief */
    var b = Array.ofDim[Double](dim)

    /* external message for this node. Will usually not be updated during inference */
    var in = Array.ofDim[Double](dim)

    /* the domain of values. By default this corresponds to [0,dim) but can be a subset if observations are given */
    var domain: Array[Int] = _

    /* indicates that variable is in a certain state */
    var setting: Int = 0

    /* indicates the value corresponding to the setting of the node */
    var value: Int = 0

    def toVerboseString(nodePrinter: Node => String = n => "") = {
      f"""-----------------
        |Node:   $index%3d ${ nodePrinter(this) }
        |Belief:
        |${ b.mkString("\n") }
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
  final class Edge(val n: Node, val f: Factor, val dim: Int) {
    val n2f     = Array.ofDim[Double](dim)
    val f2n     = Array.ofDim[Double](dim)
    val f2nLast = Array.ofDim[Double](dim)

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
   * A generic message passing interface for terms that have structure and give rise to more efficient
   * inference algorithms.
   */
  trait StructuredPotential {
    def score(factor: Factor, setting: Array[Int], weights: FactorieVector): Double
    def maxMarginal2Node(edge: Edge)
    def maxMarginal2AllNodes(factor: Factor)
    def argmaxMarginal2AllNodes(factor: Factor): Array[Int]
    def maxScoreAndFeatures(factor: Factor, featureDest: FactorieVector)
  }

  /**
   * A canonical ordering of edges. Designed so that on a chain with variables indexed in order, the
   * edge order resembles forward-backward.
   */
  object EdgeOrdering extends Ordering[Edge] {
    def compare(x1: Edge, x2: Edge): Int = {
      if (x1.n.dim == 1 && x2.n.dim != 1) return 1 //messages to observed nodes should be last
      if (x2.n.dim == 1 && x1.n.dim != 1) return -1
      if (x1.f.numNeighbors != x2.f.numNeighbors) return x1.f.numNeighbors - x2.f.numNeighbors
      if (x1.indexInFactor != x2.indexInFactor) return x2.indexInFactor - x1.indexInFactor
      val sign = -1 + (x1.indexInFactor % 2) * 2
      if (x1.n.index != x2.n.index) return sign * (x1.n.index - x2.n.index)
      x1.f.index - x2.f.index
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