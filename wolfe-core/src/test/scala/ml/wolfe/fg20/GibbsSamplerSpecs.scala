package ml.wolfe.fg20

import cc.factorie.la.{DenseTensor1, SingletonTensor1}
import ml.wolfe.WolfeSpec

import scala.util.Random

/**
 * @author Sebastian Riedel
 */
class GibbsSamplerSpecs extends WolfeSpec {

  "A Gibbs Sampler" should {
    "converge to true marginals" ignore {
      implicit val random = new Random(0)
      val booleans = Seq(false, true)
      val v1 = new DiscVar(booleans, "v1")
      val v2 = new DiscVar(booleans, "v2")
      val v3 = new DiscVar(booleans, "v3")
      val stats = LinearPotential.stats(Array(2, 2), {
        case Array(x1, x2) => new SingletonTensor1(4, x1 * 2 + x2, 1.0)
      })
      val weightsVar = new VectVar(name = "w")
      val weights = new DenseTensor1(Array(0.0, 0.0, 0.0, 0.0))
      val pots = Seq(
        new LinearPotential2(Array(v1, v2), weightsVar, stats),
        new LinearPotential2(Array(v2, v3), weightsVar, stats),
        new LinearPotential2(Array(v3, v1), weightsVar, stats),
        new TablePotential2(Array(v1), Array(0.0, 1.0)),
        new TablePotential2(Array(v2), Array(0.0, 1.0))
      )
      val problem = Problem(pots)
      val sampler = new GibbsSampler(problem)
      val result = sampler.inferMarginals(10000, 0, weights)
      val expected = new BruteForce(problem).inferMarginals()
      for (v <- problem.discVars) {
        for (i <- v.dom)
          result.marginals.discBelief(v).prob(i) should be (expected.marginals.discBelief(v).prob(i) +- 0.01)
      }
    }
  }

}
