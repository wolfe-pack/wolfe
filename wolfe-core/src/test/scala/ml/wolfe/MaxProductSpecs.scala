package ml.wolfe

import ml.wolfe.potential.{Table, TablePotential}
import scala.util.Random

/**
 * @author Sebastian Riedel
 */
class MaxProductSpecs extends WolfeSpec {

  import FactorGraph._

  val randomTable = TablePotential.table(Array(2, 2), _ => Random.nextGaussian())
  val fixedTable  = TablePotential.table(Array(2, 2), {
    case Array(0, 0) => 1
    case Array(0, 1) => 2
    case Array(1, 0) => -3
    case Array(1, 1) => 0
  })

  def tablePotential(fg: FactorGraph, n1: Node, n2: Node, table: Table) = {
    val f1 = fg.addFactor()
    val e1 = fg.addEdge(f1, n1)
    val e2 = fg.addEdge(f1, n2)
    f1.potential = TablePotential(Array(e1, e2), table)
  }

  def oneFactorFG() = {
    val fg = new FactorGraph
    val n1 = fg.addNode(2)
    val n2 = fg.addNode(2)
    tablePotential(fg, n1, n2, fixedTable)
    fg.build()
    fg
  }

  def chainFG(length:Int) = {
    val fg = new FactorGraph
    val nodes = for (i <- 0 until length) yield fg.addNode(2)
    for ((n1,n2) <- nodes.dropRight(1) zip nodes.drop(1)) tablePotential(fg,n1,n2,fixedTable)
    fg.build()
    fg
  }


  def sameBeliefs(fg1: FactorGraph, fg2: FactorGraph) = {
    def sameBeliefs(n1: List[FactorGraph.Node], n2: List[FactorGraph.Node]): Boolean = (n1, n2) match {
      case (Nil, Nil) => true
      //todo: this should be approx. equal on array
      case (h1 :: t1, h2 :: t2) => MoreArrayOps.approxEqual(h1.b, h2.b) && sameBeliefs(t1, t2)
      case _ => false
    }
    sameBeliefs(fg1.nodes.toList, fg2.nodes.toList)
  }

  "A Max Product algorithm" should {
    "return the exact max-marginals when given a single table potential" in {
      val fg_mp = oneFactorFG()
      val fg_bf = oneFactorFG()

      MaxProductNew(fg_mp, 1)
      BruteForceSearchNew(fg_bf)

      sameBeliefs(fg_mp, fg_bf) should be(true)
    }
    "return the exact marginals given a chain" in {
      val fg_mp = chainFG(5)
      val fg_bf = chainFG(5)

      MaxProductNew(fg_mp, 1)
      BruteForceSearchNew(fg_bf)

      sameBeliefs(fg_mp, fg_bf) should be(true)

    }
  }

}
