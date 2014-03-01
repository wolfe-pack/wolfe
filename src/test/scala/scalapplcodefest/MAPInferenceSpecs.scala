package scalapplcodefest

import org.scalatest.{Matchers, WordSpec}
import scala.util.Random
import scalapplcodefest.MPGraph.FactorType
import scalapplcodefest.TermDSL._
import scala.Unit
import scalapplcodefest.legacy.value.{Doubles, Bools}

/**
 *
 *
 * @author svivek
 */
class MAPInferenceSpecs extends WordSpec with Matchers {
  implicit val random = new Random(0)

  def buildFactorTable(states: Array[Array[Int]]): MPGraph = {
    val table = states.map {_ => random.nextGaussian()}.toArray

    val numNodes = states(0).length

    val fg = new MPGraph
    val f = fg.addTableFactor(table, states, (0 until numNodes).map {_ => 2}.toArray)

    val edges = (0 until numNodes).map {
      n => {
        val node = fg.addNode(2)

        val e = fg.addEdge(f, node, n)

        // TODO this is ugly! fg.addEdge should have registered the edge with the node
        node.edges = Array(e)
        e
      }
    }.toArray

    // TODO this is ugly! fg.addEdge should have registered the edges with the factor
    f.edges = edges

    fg
  }


  def handBuiltSingleTableFactorMAP(algorithm: MPGraph => Unit, allStates: Array[Array[Int]]) = {
    s"find argmax for a hand-built factor graph with ${allStates(0).length} nodes" in {
      val fg = buildFactorTable(allStates)

      val table = fg.factors(0).table

      val solutionId = (0 until table.size).maxBy(i => table(i))
      val solution = allStates(solutionId)

      algorithm(fg)

      for (nodeId <- 0 until fg.nodes.size) {
        val node = fg.nodes(nodeId)
        val assignment = solution(nodeId)
        val prediction = (0 until node.b.size).maxBy(node.b(_))
        prediction should be(assignment)
      }
    }
  }

  def compiledSingleTableFactorMAP(algorithm: MPGraph => Unit) = {
    "find argmax for a compiled factor graph with one factor and one variable" in {
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
    "find argmax for a compiled factor graph with two factors and one variable" in {
      val x = 'x of bools
      val p1 = random.nextGaussian()
      val n1 = random.nextGaussian()

      val f1 = fun(Map(true -> p1, false -> n1), Bools, Doubles)(x)

      val n2 = random.nextGaussian()
      val p2 = random.nextGaussian()

      val f2 = fun(Map(true -> p2, false -> n2), Bools, Doubles)(x)

      val model = f1 + f2

      val compiled = MPGraphCompiler.compile(x, model)

      val fg = compiled.graph

      val posWeight = p1 + p2
      val negWeight = n1 + n2
      val solution = if (posWeight > negWeight) 1 else 0

      algorithm(fg)
      val node = fg.nodes(0)
      (0 until node.b.size).maxBy(node.b(_)) should be(solution)
    }
  }

  def compiledTwoTableTwoNodeFactorMAP(algorithm: MPGraph => Unit) = {
    "find argmax for a compiled factor graph with two factors and two variables" in {
      val x = 'x of bools
      val p1 = random.nextGaussian()
      val n1 = random.nextGaussian()

      val f1 = fun(Map(true -> p1, false -> n1), Bools, Doubles)(x)

      val y = 'y of bools
      val xyMap = Map(
        (true, true) -> random.nextGaussian(),
        (true, false) -> random.nextGaussian(),
        (false, true) -> random.nextGaussian(),
        (false, false) -> random.nextGaussian())
      val f2 = fun(xyMap, xyMap.keySet)((x, y))

      val model = f1 + f2

      val compiled = MPGraphCompiler.compile(sig(x, y), model)

      val fg = compiled.graph

      println(fg.toVerboseString())


      // what follows is stream of consciousness code that finds the highest scoring assignment for this factor graph.
      val partition = fg.factors.partition {factor => factor.edges.size == 2}
      val xyFactor = partition._1.head
      val xFactor = partition._2.head


      val xNode = xFactor.edges(0).n
      val xNodeId = xyFactor.edges.zipWithIndex.filter(_._1.n == xNode).head._2
      val yNodeId = 1 - xNodeId // hackity hack

      val yNode = xyFactor.edges(yNodeId).n

      val bestSettingId = (0 until xyFactor.settings.size).maxBy {
        settingId => {
          val setting = xyFactor.settings(settingId)

          val xSetting = setting(xNodeId)
          val ySetting = setting(yNodeId)

          xyFactor.table(xSetting) + xyFactor.table(ySetting) + xFactor.table(xSetting)

        }
      }

      algorithm(fg)

      val bestSetting = xyFactor.settings(bestSettingId)

      val xPrediction = (0 until xNode.b.size).maxBy(xNode.b(_))
      val yPrediction = (0 until yNode.b.size).maxBy(yNode.b(_))

      bestSetting(xNodeId) should be(xPrediction)

      bestSetting(yNodeId) should be(yPrediction)

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

  val oneNodeStates = Array(Array(0), Array(1))
  val twoNodeStates = Array(Array(0, 0), Array(0, 1), Array(1, 0), Array(1, 1))
  val threeNodeStates = Array(Array(0, 0, 0), Array(0, 0, 1), Array(0, 1, 0), Array(0, 1, 1), Array(1, 0, 0),
    Array(1, 0, 1), Array(1, 1, 0), Array(1, 1, 1))


  "Table maxer" when {
    "given a single binomial factor" should {
      behave like handBuiltSingleTableFactorMAP(tableMaxer, oneNodeStates)
      behave like handBuiltSingleTableFactorMAP(tableMaxer, twoNodeStates)
      behave like handBuiltSingleTableFactorMAP(tableMaxer, threeNodeStates)

      behave like compiledSingleTableFactorMAP(tableMaxer)
    }
  }

  "MaxProduct" when {
    "given single a binomial factor" should {
      behave like handBuiltSingleTableFactorMAP(maxProduct, oneNodeStates)
      behave like handBuiltSingleTableFactorMAP(maxProduct, twoNodeStates)
      behave like handBuiltSingleTableFactorMAP(maxProduct, threeNodeStates)

      behave like compiledSingleTableFactorMAP(maxProduct)
    }
    "given a single variable with two factors" should {

      behave like compiledTwoTableFactorMAP(maxProduct)
    }
    "given a model with two variables and two factors" should {
      behave like compiledTwoTableTwoNodeFactorMAP(maxProduct)
    }
  }

  "Dual decomposition" when {
    "given given a single binomial factor" should {
      behave like handBuiltSingleTableFactorMAP(dualDecomposition, oneNodeStates)
      behave like handBuiltSingleTableFactorMAP(dualDecomposition, twoNodeStates)
      behave like handBuiltSingleTableFactorMAP(dualDecomposition, threeNodeStates)

      behave like compiledSingleTableFactorMAP(dualDecomposition)
    }
    "given a single variable with two factors" should {
      behave like compiledTwoTableFactorMAP(dualDecomposition)
    }
    "given a model with two variables and two factors" should {
      behave like compiledTwoTableTwoNodeFactorMAP(dualDecomposition)
    }
  }

}
