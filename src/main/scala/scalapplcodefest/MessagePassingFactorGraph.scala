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
final class MessagePassingFactorGraph {

  import MessagePassingFactorGraph.FactorType._
  import MessagePassingFactorGraph._

  val edges = new ArrayBuffer[Edge]
  val nodes = new ArrayBuffer[Node]
  val factors = new ArrayBuffer[Factor]
  var weights: DenseVector = null

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
  def addEdge(f: Factor, n: Node, indexInFactor:Int) = {
    val e = new Edge(n, f, n.dim)
    e.indexInFactor = indexInFactor
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
      edge.indexInNode = edge.n.edgeFilled
      edge.f.edges(edge.indexInFactor) = edge
      edge.n.edges(edge.indexInNode) = edge
      edge.f.edgeFilled += 1
      edge.n.edgeFilled += 1
    }
  }


  def toVerboseString(fgPrinter:FGPrinter = DefaultPrinter) = {
    f"""
      |Nodes:
      |${nodes.map(_.toVerboseString(fgPrinter.node2String)).mkString("\n")}
      |
      |Factors:
      |${factors.map(_.toVerboseString(fgPrinter)).mkString("\n")}
      |
      |Edges:
      |${edges.map(_.toVerboseString(fgPrinter)).mkString("\n")}
    """.stripMargin
  }
  def addFactor1(table: Array[Double]) = {
    val f = new Factor(this, factors.size, Array(table.length), Range(0, table.length).map(Array(_)).toArray, TABLE, table)
    factors += f
    f
  }

  def addTableFactor(scores: Array[Double], settings: Array[Array[Int]], dims: Array[Int]) = {
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

object MessagePassingFactorGraph {

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

    /* external message for this node. Will usually not be updated during inference */
    val in = Array.ofDim[Double](dim)

    def toVerboseString(nodePrinter:Node => String = n => "") = {
      f"""-----------------
        |Node:   $index%3d ${nodePrinter(this)}
        |Belief:
        |${b.mkString("\n")}
      """.stripMargin
    }

    override def toString = index.toString

    private[MessagePassingFactorGraph] var edgeCount: Int = 0
    private[MessagePassingFactorGraph] var edgeFilled: Int = 0

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

    def toVerboseString(fgPrinter:FGPrinter) =
      f"""----------
        |Edge
        |Node:    ${n.index} ${fgPrinter.node2String(n)}
        |Factor:  ${f.index} ${fgPrinter.factor2String(f)}
      """.stripMargin
    override def toString = s"${f.index} -> ${n.index}"
  }

  /**
   * A factor in a message passing factor graph.
   * @param fg the factor graph.
   * @param index the index/id of the factor
   * @param dims array with dimensions of the participating variables, in the order of the `edges` array of edges.
   * @param settings array with integer array representations of settings of the neighbors
   * @param typ the type of factor
   * @param table if `typ=Table` this stores a score for each possible setting (indexed by the index of the setting in
   *              `settings`.
   * @param stats if `typ=Linear` this stores a feature vector for each setting (index by the index of the setting
   *              in `settings`.
   */
  final class Factor(val fg: MessagePassingFactorGraph, val index: Int, val dims: Array[Int], val settings: Array[Array[Int]],
                     val typ: FactorType.Value = MessagePassingFactorGraph.FactorType.TABLE,
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
    def toVerboseString(implicit fgPrinter: FGPrinter) = {
      val tableString = typ match {
        case TABLE =>
          for ((setting, index) <- settings.zipWithIndex) yield
            s"${setting.mkString(" ")} | ${table(index)}"
        case LINEAR =>
          for ((setting, index) <- settings.zipWithIndex) yield
            s"${setting.mkString(" ")} | ${score(index)} | ${fgPrinter.vector2String(stats(index))}"

      }
      f"""-----------------
        |Factor:  $index ${fgPrinter.factor2String(this)}
        |Nodes:   ${edges.map(_.n.index).mkString(" ")} ${edges.map(e => fgPrinter.node2String(e.n)).mkString(" ")}
        |Type:    $typ
        |Table:
        |${tableString.mkString("\n")}
      """.stripMargin
    }

    override def toString = index.toString

    /**
     * Calculates scores in table based on feature vectors and currently set weights.
     */
    def cacheLinearScores() {
      for (i <-0 until settings.length) table(i) = stats(i).dot(fg.weights)
    }

    private[MessagePassingFactorGraph] var edgeCount: Int = 0
    private[MessagePassingFactorGraph] var edgeFilled: Int = 0

  }

  /**
   * Printer of nodes, factors and edges. Helps debugging (as the graph itself is
   * independent of the original model it comes from.
   */
  trait FGPrinter {
    def node2String(node:Node):String
    def factor2String(factor:Factor):String
    def vector2String(vector:Vector):String
  }

  object DefaultPrinter extends FGPrinter {
    def node2String(node: Node) = ""
    def factor2String(factor: Factor) = ""
    def vector2String(vector: scalapplcodefest.Vector) = vector.toString()
  }


}