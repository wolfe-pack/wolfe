package ml.wolfe

import ml.wolfe.potential.TablePotential
import scala.util.Random

/**
 * @author Sebastian Riedel
 */
class MaxProductSpecs extends WolfeSpec {

  val randomTable = TablePotential.table(Array(2,2),_ => Random.nextGaussian())

  def tableFG() = {
    val fg = new FactorGraph
    val n1 = fg.addNode(2)
    val n2 = fg.addNode(2)
    val f1 = fg.addFactor()
    val e1 = fg.addEdge(f1, n1)
    val e2 = fg.addEdge(f1, n2)
    f1.potential = TablePotential(Array(e1,e2), randomTable)
    fg.build()
    fg
  }

  def sameBeliefs(fg1:FactorGraph, fg2:FactorGraph) = {
    def sameBeliefs(n1:List[FactorGraph.Node],n2:List[FactorGraph.Node]):Boolean = (n1,n2) match {
      case (Nil,Nil) => true
      case (h1::t1, h2::t2) => h1.b.deep == h2.b.deep && sameBeliefs(t1,t2)
      case _ => false
    }
    sameBeliefs(fg1.nodes.toList, fg2.nodes.toList)
  }

  "A Max Product algorithm" should {
    "return the exact max-marginals when given a table potential" in {
      val fg_mp = tableFG()
      val fg_bf = tableFG()

      MaxProductNew(fg_mp,1)
      BruteForceSearchNew(fg_bf)

      println(fg_mp.nodes.map(_.b.mkString(" ")))
      println(fg_bf.nodes.map(_.b.mkString(" ")))

      println(fg_mp.value)
      println(fg_bf.value)

//      sameBeliefs(fg_mp,fg_bf) should be (true)






//      //fg.pot = new TablePotential(Array(e1,e2),settings,scores)
//      trait Potential {
//        def setup(factor:FactorGraph.Factor)
//      }
    }
  }

}
