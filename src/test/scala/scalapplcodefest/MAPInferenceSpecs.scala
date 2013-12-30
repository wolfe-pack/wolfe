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

      node.b(solution) should be(1.0)
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

      fg.nodes(0).b(solution) should be(1.0)
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

  "Dual decomposition" when {
    "given a single binomial factor" should {
      behave like handBuiltSingleTableFactorMAP(dualDecomposition)

      behave like compiledSingleTableFactorMAP(dualDecomposition)
    }
  }

  "Table maxer" when {
    "single binomial factor" should {
      behave like handBuiltSingleTableFactorMAP(tableMaxer)

      behave like compiledSingleTableFactorMAP(tableMaxer)
    }
  }


}
