package ml.wolfe

import ml.wolfe.potential.TablePotential
import scala.util.Random

/**
 * @author Sebastian Riedel
 */
class MaxProductSpecs extends WolfeSpec {

  "A Max Product algorithm" should {
    "return the exact max-marginals when given a table potential" in {
      val fg = new FactorGraph
      val n1 = fg.addNode(2)
      val n2 = fg.addNode(2)
      val f1 = fg.addFactor()
      val e1 = fg.addEdge(f1, n1)
      val e2 = fg.addEdge(f1, n2)
      f1.potential = TablePotential(Array(e1,e2), _ => Random.nextGaussian())

      fg.build()



//      //fg.pot = new TablePotential(Array(e1,e2),settings,scores)
//      trait Potential {
//        def setup(factor:FactorGraph.Factor)
//      }
    }
  }

}
