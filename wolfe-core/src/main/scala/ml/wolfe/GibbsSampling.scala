package ml.wolfe

import ml.wolfe.fg.{DiscreteVar, Var}

import scala.math._
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
      sample(n.variable)
      for(f <- fg.expectationFactors) {
        fg.expectations += f.potential.statsForCurrentSetting()
      }
    }

    fg.expectations /= iterations
  }

  def sample(v:Var[_]):Unit =
    if(v.node.edges.isEmpty) sampleUniform(v)
    else {
      def score = exp(v.node.edges.map(_.f.potential.valueForCurrentSetting()).sum)

      val oldSetting = v.setting
      val oldScore = score

      val e = v.node.edges(Random.nextInt(v.node.edges.length))
      e.f.potential.proposeSetting(e)
      val newScore = score

      if(newScore / oldScore < Math.random()) v.setting = oldSetting
    }


  def sampleUniform(v:Var[_]): Unit = v match {
    case v:DiscreteVar[_] => v.setting = Random.nextInt(v.dim)
  }

}
