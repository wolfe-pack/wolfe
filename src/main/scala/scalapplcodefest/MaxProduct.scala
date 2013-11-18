package scalapplcodefest

import scalaxy.loops._
import java.util
import cc.factorie.maths.ArrayOps


/**
 * @author Sebastian Riedel
 */
object MaxProduct {

  import MessagePassingGraph._

  def main(args: Array[String]) {
    val fg = new MessagePassingGraph
    val f1 = fg.addFactor2(Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0)))
    val n1 = fg.addNode(2)
    val n2 = fg.addNode(3)
    val e1 = fg.addEdge(f1, n1)
    val e2 = fg.addEdge(f1, n2)
    fg.build()

    MaxProduct.run(fg, 1)

    println(n1.b.mkString(" "))
    println(n2.b.mkString(" "))

  }

  def run(fg: MessagePassingGraph, maxIteration: Int) {
    for (i <- (0 until maxIteration).optimized) {
      for (edge <- fg.edges) {
        updateN2F(edge)
        updateF2N(edge)
      }
    }
    for (node <- fg.nodes) updateBelief(node)
  }

  def objective(fg: MessagePassingGraph): Double = {
    ???
  }

  def featureExpectations(fg: MessagePassingGraph): SparseVector = {
    val result = new SparseVector(100)
    for (factor <- fg.factors; if factor.typ == MessagePassingGraph.FactorType.LINEAR) {
      for (i <- 0 until factor.entryCount) {
        val setting = factor.settings(i)
        var score = factor.score(i)
      }
    }
    ???
  }

  def updateF2N(edge: Edge) {
    val factor = edge.f
    util.Arrays.fill(edge.f2n, Double.MinValue)
    for (i <- 0 until factor.entryCount) {
      val setting = factor.settings(i)
      var score = factor.score(i)
      val varValue = setting(edge.indexInFactor)
      for (j <- 0 until factor.rank; if j != edge.indexInFactor) {
        score += factor.edges(j).n2f(setting(j))
      }
      edge.f2n(varValue) = math.max(score, edge.f2n(varValue))
    }
  }

  def updateN2F(edge: Edge) {
    val node = edge.n
    System.arraycopy(node.in, 0, edge.n2f, 0, edge.n2f.length)
    for (i <- 0 until node.dim) {
      for (e <- 0 until node.edges.length; if e != edge.indexInNode)
        edge.n2f(i) += node.edges(e).f2n(i)
    }
  }

  def updateBelief(node: Node) {
    System.arraycopy(node.in, 0, node.b, 0, node.b.length)
    for (e <- 0 until node.edges.length)
      for (i <- 0 until node.dim)
        node.b(i) += node.edges(e).f2n(i)
  }

  def updateAssignment(node: Node) {
    var max = Double.MinValue
    for (i <- 0 until node.dim)
      max = math.max(max,node.b(i))
    for (i <- 0 until node.dim) {
      node.a(i) = if (node.b(i) == max) 0.0 else Double.NegativeInfinity
    }
  }

}







