package ml.wolfe

import ml.wolfe.fg._
import scala.util.Random
import ml.wolfe.fg.Table
import ml.wolfe.fg.Stats

/**
 * @author Sebastian Riedel
 */
class BeliefPropagationSpecs extends WolfeSpec {

  import FactorGraph._

  val pot: Array[Int] => Double = {
    case Array(0, 0) => 1
    case Array(0, 1) => 2
    case Array(1, 0) => -3
    case Array(1, 1) => 0
  }

  val fixedTable = TablePotential.table(Array(2, 2), pot)

  val xorPot: Array[Int] => Double = arr => if ((arr(0)==1) ^ (arr(1)==1)) 0 else Double.NegativeInfinity
  val xorTable = TablePotential.table(Array(2, 2), xorPot)

  val fixedStats = LinearPotential.stats(Array(2, 2), {
    case Array(i, j) => LinearPotential.singleton(2 * i + j, 1.0)
  })


  def tablePotential(fg: FactorGraph, n1: Node, n2: Node, table: Table) = {
    val f1 = fg.addFactor()
    val e1 = fg.addEdge(f1, n1)
    val e2 = fg.addEdge(f1, n2)
    f1.potential = TablePotential(Array(e1, e2), table)
    f1
  }

  def linearPotential(fg: FactorGraph, n1: Node, n2: Node, stats: Stats) = {
    val f1 = fg.addFactor()
    val e1 = fg.addEdge(f1, n1)
    val e2 = fg.addEdge(f1, n2)
    f1.potential = new LinearPotential(Array(e1, e2), stats, fg)
    f1
  }

  def oneFactorFG() = {
    val fg = new FactorGraph
    val n1 = fg.addDiscreteNode(2)
    val n2 = fg.addDiscreteNode(2)
    tablePotential(fg, n1, n2, fixedTable)
    fg.build()
    fg
  }

  def xorFG() = {
    val fg = new FactorGraph
    val n1 = fg.addDiscreteNode(2)
    val n2 = fg.addDiscreteNode(2)
    tablePotential(fg, n1, n2, xorTable)
    fg.build()
    fg
  }

  def chainFG(length: Int) = {
    val fg = new FactorGraph
    val nodes = for (i <- 0 until length) yield fg.addDiscreteNode(2)
    for ((n1, n2) <- nodes.dropRight(1) zip nodes.drop(1)) tablePotential(fg, n1, n2, fixedTable)
    fg.build()
    fg
  }

  def chainFGWithFeatures(length: Int) = {
    val fg = new FactorGraph
    val nodes = for (i <- 0 until length) yield fg.addDiscreteNode(2)
    for ((n1, n2) <- nodes.dropRight(1) zip nodes.drop(1)) linearPotential(fg, n1, n2, fixedStats)
    fg.weights = LinearPotential.dense(4, 0 -> 1.0, 1 -> 2.0, 2 -> -3, 3 -> 0)
    fg.build()
    fg
  }


  def sameBeliefs(fg1: FactorGraph, fg2: FactorGraph) = {
    def sameBeliefs(n1: List[FactorGraph.Node], n2: List[FactorGraph.Node]): Boolean = (n1, n2) match {
      case (Nil, Nil) => true
      //todo: this should be approx. equal on array
      case (h1 :: t1, h2 :: t2) =>
        MoreArrayOps.approxEqual(h1.variable.asDiscrete.b, h2.variable.asDiscrete.b) && sameBeliefs(t1, t2)
      case _ => false
    }
    sameBeliefs(fg1.nodes.toList, fg2.nodes.toList)
  }

  def sameVector(v1: FactorieVector, v2: FactorieVector, eps: Double = 0.00001) = {
    v1.activeDomain.forall(i => math.abs(v1(i) - v2(i)) < eps) &&
    v2.activeDomain.forall(i => math.abs(v1(i) - v2(i)) < eps)
  }

  "A Max Product algorithm" should {
    "return the exact max-marginals when given a single table potential" in {
      val fg_mp = oneFactorFG()
      val fg_bf = oneFactorFG()

      BeliefPropagation(fg_mp, 1)
      BruteForce.maxMarginals(fg_bf)

      sameBeliefs(fg_mp, fg_bf) should be(true)
      fg_mp.value should be(fg_bf.value)

    }
    "choose a valid global max from a factor graph with multiple solutions" in {
      val fg = xorFG()
      BeliefPropagation(fg, 1)
      val v0 = fg.nodes(0).variable.asDiscrete
      val v1 = fg.nodes(1).variable.asDiscrete
      v0.setting should not be(v1.setting)
    }
    "return the exact marginals given a chain" in {
      val fg_mp = chainFG(5)
      val fg_bf = chainFG(5)

      BeliefPropagation(fg_mp, 1)
      BruteForce.maxMarginals(fg_bf)

      sameBeliefs(fg_mp, fg_bf) should be(true)
      fg_mp.value should be(fg_bf.value)

    }
    "return feature vectors of argmax state" in {
      val fg_mp = chainFGWithFeatures(5)
      val fg_bf = chainFGWithFeatures(5)

      BeliefPropagation(fg_mp, 1)
      BruteForce.maxMarginals(fg_bf)

      sameBeliefs(fg_mp, fg_bf) should be(true)
      sameVector(fg_mp.gradient, fg_bf.gradient) should be (true)
      fg_mp.value should be(fg_bf.value)
    }

  }

  "A BrufeForce algorithm" should {
    "return the exact log partition function" in {
      import math._
      val fg_bf = oneFactorFG()
      BruteForce.marginalize(fg_bf)
      val logZ = log(fixedTable.scores.map(exp).sum)
      fg_bf.value should be(logZ)
    }
  }

  "A Sum Product algorithm" should {
    "return the exact marginals when given a single table potential" in {
      val fg_bp = oneFactorFG()
      val fg_bf = oneFactorFG()

      BeliefPropagation.sumProduct(1)(fg_bp)
      BruteForce.marginalize(fg_bf)

      fg_bp.value should be(fg_bf.value)
      sameBeliefs(fg_bp, fg_bf) should be(true)


    }
    "return the exact marginals given a chain" in {
      val fg_bp = chainFG(5)
      val fg_bf = chainFG(5)

      BeliefPropagation.sumProduct(1)(fg_bp)
      BruteForce.marginalize(fg_bf)

      sameBeliefs(fg_bp, fg_bf) should be(true)
      fg_bp.value should be(fg_bf.value +- 0.001)

    }
    "return feature vectors of argmax state" in {
      val fg_bp = chainFGWithFeatures(5)
      val fg_bf = chainFGWithFeatures(5)

      BeliefPropagation.sumProduct(1)(fg_bp)
      BruteForce.marginalize(fg_bf)

      sameBeliefs(fg_bp, fg_bf) should be(true)
      sameVector(fg_bp.gradient, fg_bf.gradient) should be (true)
      fg_bp.value should be(fg_bf.value +- 0.001)


    }

  }


}
