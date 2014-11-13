package ml.wolfe.fg20

import cc.factorie.la.{DenseTensor1, SingletonTensor1}
import ml.wolfe.WolfeSpec

import scala.util.Random

/**
 * @author Sebastian Riedel
 */
class GibbsSamplerSpecs extends WolfeSpec {

  "A Gibbs Sampler" should {
    "converge to true marginals" in {
      implicit val random = new Random(0)
      val booleans = Seq(false, true)
      val v1 = new DiscVar(booleans, "v1")
      val v2 = new DiscVar(booleans, "v2")
      val v3 = new DiscVar(booleans, "v3")
      val stats = LinearPotential.stats(Array(2, 2), {
        case Array(x1, x2) => new SingletonTensor1(4, x1 * 2 + x2, 1.0)
      })
      val weights = new DenseTensor1(Array(0.0, 1.0, 1.0, 0.0))
      val pots = Seq(
        new LinearPotential(Array(v1, v2), stats),
        new LinearPotential(Array(v2, v3), stats),
        new LinearPotential(Array(v3, v1), stats),
        new TablePotential(Array(v1), Array(0.0, 5.0)),
        new TablePotential(Array(v2), Array(0.0, 3.0))
      )
      val problem = Problem(pots, Seq(v1, v2, v3))
      val sampler = new GibbsSampler(problem)
      val result = sampler.inferMarginals(1000, 0, weights)
      val expected = new BruteForce(problem).inferMarginals(weights)
      println(result.marginals)
      println(expected.marginals)

    }
  }

}
