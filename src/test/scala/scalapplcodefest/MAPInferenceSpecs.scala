package scalapplcodefest

import org.scalatest.{Matchers, WordSpec}
import scala.util.Random
import scalapplcodefest.MPGraph.FactorType
import scalapplcodefest.TermDSL._
import scala.Unit
import scalapplcodefest.value.{Doubles, Bools}

/**
 *
 *
 * @author svivek
 */
class MAPInferenceSpecs extends WordSpec with Matchers {
  implicit val random = new Random(0)

  def handBuiltSingleTableFactorMAP(algorithm: MPGraph => Unit) = {
    "find argmax for a hand-built factor graph" in {
      val fg = new MPGraph

      val table = Array(random.nextGaussian(), random.nextGaussian())

      val f = fg.addTableFactor(table, Array(Array(0), Array(1)), Array(2))
      val node = fg.addNode(2)

      val e = fg.addEdge(f, node, 0)

      f.edges = Array(e)
      node.edges = Array(e)

      val solution = if (table(0) > table(1)) 0 else 1

      algorithm(fg)

      (0 until node.b.size).maxBy(node.b(_)) should be(solution)

    }
  }

  def compiledSingleTableFactorMAP(algorithm: MPGraph => Unit) = {
    "find argmax for a compiled factor graph" in {
      val x = 'x of bools
      val posWeight = random.nextGaussian()
      val negWeight = random.nextGaussian()
      val t = fun(Map(true -> posWeight, false -> negWeight), Bools, Doubles)(x)

      val compiled = MPGraphCompiler.compile(x, t)
      val fg = compiled.graph

      val solution = if (posWeight > negWeight) 1 else 0

      algorithm(fg)

      val node = fg.nodes(0)
      (0 until node.b.size).maxBy(node.b(_)) should be(solution)
    }
  }

  def compiledTwoTableFactorMAP(algorithm: MPGraph => Unit) = {
    "find argmax for a compiled factor graph" in {
      val x = 'x of bools
      val p1 = random.nextGaussian()
      val n1 = random.nextGaussian()

      val f1 = fun(Map(true -> p1, false -> n1), Bools, Doubles)(x)

      val p2 = random.nextGaussian()
      val n2 = random.nextGaussian()

      val f2 = fun(Map(true -> p2, false -> n2), Bools, Doubles)(x)

      val model = f1 + f2

      val compiled = MPGraphCompiler.compile(x, model)

      val fg = compiled.graph

      val posWeight = p1 + p2
      val negWeight = p1 + p2
      val solution = if (posWeight > negWeight) 1 else 0

      algorithm(fg)
      val node = fg.nodes(0)
      (0 until node.b.size).maxBy(node.b(_)) should be(solution)
    }
  }

  def tableMaxer(fg: MPGraph) = {
    // assuming a factor graph with one table
    if (fg.factors.size == 1 && fg.factors(0).typ == FactorType.TABLE) {
      val factor = fg.factors(0)

      val best = (0 until factor.settings.size) maxBy {i => factor.table(i)}

      val argmax = factor.settings(best)

      for (i <- 0 until argmax.size) {
        val node = factor.edges(i).n
        node.b(argmax(i)) = 1
      }
    }
  }

  def dualDecomposition(fg: MPGraph) = DualDecomposition(fg, 100)

  def maxProduct(fg: MPGraph) = MaxProduct(fg, 100)

  "Table maxer" when {
    "given a single binomial factor" should {
      behave like handBuiltSingleTableFactorMAP(tableMaxer)

      behave like compiledSingleTableFactorMAP(tableMaxer)
    }
  }

  "MaxProduct" when {
    "given single a binomial factor" should {
      behave like handBuiltSingleTableFactorMAP(maxProduct)

      behave like compiledSingleTableFactorMAP(maxProduct)
    }
    "given a single variable with two factors" should {
      behave like compvedTwoTableFactorMAP(maxProduct)
    }
  }

  "Dual decomposition" when {
    "given given a single binomial factor" should {
      behave like handBuiltSingleTableFactorMAP(dualDecomposition)

      behave like compiledSingleTableFactorMAP(dualDecomposition)
    }
    "given a single variable with two factors" should {
      behave like compiledTwoTableFactorMAP(dualDecomposition)
    }
  }
}
