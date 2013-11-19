package scalapplcodefest

import scala.collection.mutable.ArrayBuffer
import scalaxy.loops._


/**
 * A simple Factor Graph with infrastructure for message passing.
 * It stores messages and table potentials in double arrays.
 * Factors can have one of three types:
 * (1) simple dense table based;
 * (2) based on a feature vector for each state that is multiplied with a shared weight vector
 * (3) generic (calls a function to be evaluated at a particular state)
 */
final class MessagePassingGraph {

  import MessagePassingGraph.FactorType._
  import MessagePassingGraph._

  val edges = new ArrayBuffer[Edge]
  val nodes = new ArrayBuffer[Node]
  val factors = new ArrayBuffer[Factor]
  var weights: DenseVector = null

  def addNode(dim: Int) = {
    val n = new Node(nodes.size, dim)
    nodes += n
    n
  }

  def addEdge(f: Factor, n: Node) = {
    val e = new Edge(n, f, n.dim)
    n.edgeCount += 1
    f.edgeCount += 1
    edges += e
    e
  }

  /**
   * Connecting nodes and factors to the edges between them.
   */
  def build() {
    for (edge <- edges) {
      if (edge.f.edges == null) edge.f.edges = Array.ofDim[Edge](edge.f.edgeCount)
      if (edge.n.edges == null) edge.n.edges = Array.ofDim[Edge](edge.n.edgeCount)
      edge.indexInFactor = edge.f.edgeFilled
      edge.indexInNode = edge.n.edgeFilled
      edge.f.edges(edge.indexInFactor) = edge
      edge.n.edges(edge.indexInNode) = edge
      edge.f.edgeFilled += 1
      edge.n.edgeFilled += 1
    }
  }


  def toVerboseString(implicit index: Index = null) = {
    """
      |Nodes:
      |%s
      |
      |Factors:
      |%s
    """.stripMargin.format(nodes.mkString("\n"), factors.map(_.toVerboseString(index)).mkString("\n"))
  }
  def addFactor1(table: Array[Double]) = {
    val f = new Factor(this, factors.size, Array(table.length), Range(0, table.length).map(Array(_)).toArray, TABLE, table)
    factors += f
    f
  }

  def addFactor(scores: Array[Double], settings: Array[Array[Int]], dims: Array[Int]) = {
    val f = new Factor(this, factors.size, dims, settings, TABLE, scores)
    factors += f
    f
  }

  def addLinearFactor(stats: Array[Vector], settings: Array[Array[Int]], dims: Array[Int]) = {
    val f = new Factor(this, factors.size, dims, settings, LINEAR, null, stats)
    factors += f
    f
  }


  def addFactor2(table: Array[Array[Double]]) = {
    val dims = Array(table.length, table(0).length)
    val entryCount = dims(0) * dims(1)
    val scores = Array.ofDim[Double](entryCount)
    val settings = Array.ofDim[Int](entryCount, dims.length)

    for (i1 <- 0 until dims(0);
         i2 <- 0 until dims(1)) {
      val entry = i1 + i2 * dims(0)
      scores(entry) = table(i1)(i2)
      settings(entry) = Array(i1, i2)
    }
    val f = new Factor(this, factors.size, dims, settings, TABLE, scores)
    factors += f
    f
  }

  def addFactor3(table: Array[Array[Array[Double]]]) = {
    val dims = Array(table.length, table(0).length, table(0)(0).length)
    val entryCount = dims(0) * dims(1) * dims(2)
    val scores = Array.ofDim[Double](entryCount)
    val settings = Array.ofDim[Int](entryCount, dims.length)

    for (i1 <- 0 until dims(0);
         i2 <- 0 until dims(1);
         i3 <- 0 until dims(2)) {
      val entry = i1 + i2 * dims(0) + i3 * dims(1) * dims(0)
      scores(entry) = table(i1)(i2)(i3)
      settings(entry) = Array(i1, i2, i3)
    }
    val f = new Factor(this, factors.size, dims, settings, TABLE, scores)
    factors += f
    f
  }
}

object MessagePassingGraph {

  /**
   * We define a set of factor types. Inference algorithms will use the type of a factor
   * to determine how to treat the factor. We use a type instead of sub-classing to
   * avoid any kind of polymorphism inside the inner loop of inference.
   */
  object FactorType extends Enumeration {
    val TABLE, LINEAR = Value
  }

  import FactorType._

  /**
   * Turns a setting vector into an entry number.
   * @param setting setting
   * @param dims dimensions of each variable.
   * @return the entry corresponding to the given setting.
   */
  final def settingToEntry(setting:Array[Int],dims:Array[Int]) = {
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
  final def entryToSetting(entry:Int,dims:Array[Int]) = {
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
  final class Node(val index: Int, val dim: Int) {
    /* all edges to factors that this node is connected to */
    var edges: Array[Edge] = null

    /* node belief */
    val b = Array.ofDim[Double](dim)

    /* node assignment */
    val a = Array.ofDim[Double](dim)

    /* external message for this node. Will usually not be updated during inference */
    val in = Array.ofDim[Double](dim)

    override def toString = {
      f"""-----------------
        |Node:   $index%d
        |Belief:
        |${b.mkString("\n")}
      """.stripMargin
    }

    private[MessagePassingGraph] var edgeCount: Int = 0
    private[MessagePassingGraph] var edgeFilled: Int = 0

  }

  /**
   * An edge between node and factor
   * @param n the node.
   * @param f the factor.
   * @param dim dimension of the node's variable.
   */
  final class Edge(val n: Node, val f: Factor, val dim: Int) {
    val n2f = Array.ofDim[Double](dim)
    val f2n = Array.ofDim[Double](dim)
    var indexInFactor = -1
    var indexInNode = -1
  }

  final class Factor(val fg: MessagePassingGraph, val index: Int, val dims: Array[Int], val settings: Array[Array[Int]],
                     val typ: FactorType.Value = MessagePassingGraph.FactorType.TABLE,
                     val table: Array[Double],
                     val stats: Array[Vector] = null) {
    var edges: Array[Edge] = null
    def rank = dims.length
    val entryCount = {
      var result = 1
      for (i <- (0 until dims.length).optimized) result *= dims(i)
      result
    }
    def score(entry: Int): Double = {
      typ match {
        case TABLE => table(entry)
        case LINEAR => stats(entry).dot(fg.weights)
      }
    }
    def toVerboseString(implicit key: Index) = {
      val tableString = typ match {
        case TABLE =>
          for ((setting, index) <- settings.zipWithIndex) yield
            s"${setting.mkString(" ")} | ${table(index)}"
        case LINEAR =>
          for ((setting, index) <- settings.zipWithIndex) yield
            s"${setting.mkString(" ")} | ${score(index)} | ${key.vectorToString(stats(index), " ")}"

      }
      f"""-----------------
        |Factor:  $index
        |Nodes:   ${edges.map(_.n.index).mkString(" ")}
        |Type:    $typ
        |Table:
        |${tableString.mkString("\n")}
      """.stripMargin
    }

    private[MessagePassingGraph] var edgeCount: Int = 0
    private[MessagePassingGraph] var edgeFilled: Int = 0

  }


}