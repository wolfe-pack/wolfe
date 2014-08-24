package ml.wolfe

import scalaxy.loops._
import scala.util.Random
import ml.wolfe.Wolfe._
/**
 * Created by luke on 22/08/14.
 */
object GibbsSampling {
  def apply(fg:FactorGraph, iterations:Int = 10000) = {
    fg.expectations = new SparseVector(1000)

    for(i <- (0 until iterations).optimized) {
      val n = fg.nodes(Random.nextInt(fg.nodes.length))
      n.variable.sample()
      for(f <- fg.expectationFactors) {
        fg.expectations += f.potential.statsForCurrentSetting()
      }
    }

    fg.expectations /= iterations
  }
}
