package ml.wolfe

import ml.wolfe.FactorGraph.Node
import ml.wolfe.fg.{CantProposeException, ContinuousVar, DiscreteVar, Var}

import scala.collection.mutable.ArrayBuffer
import scala.math._
// import scalaxy.loops._
import scala.util.Random

/**
 * Created by luke on 22/08/14.
 */
object GibbsSampling {

  def apply(fg:FactorGraph, iterations:Int = 10000, buildHistory: Boolean = false) = {
    fg.expectations = new SparseVector(10000)
    fg.visualizationSamples = new ArrayBuffer[(Var[_], Any, Boolean)]
    fg.valueHistory = new ArrayBuffer[Double]()

    val unobservedNodes = fg.nodes.filterNot(_.variable.isObserved)

    for(i <- (0 until iterations)) {
      //println( (for(n <- fg.nodes if !n.variable.isObserved) yield n.variable.label + ": " + n.variable.value).mkString("    ") + "\n")
      val n = unobservedNodes(Random.nextInt(unobservedNodes.length))
      sample(n.variable, fg)
      for (f <- fg.expectationFactors) {
        fg.expectations += f.potential.statsForCurrentSetting()
      }

      if (buildHistory) {
        fg.addValueToHistory()
      }

    }

    fg.expectations /= iterations
    Wolfe.FactorGraphBuffer.set(fg)
  }

  def sample(v:Var[_], fg:FactorGraph):Unit = {
    def score = v.node.edges.map(e => e.f.potential.valueForCurrentSetting()).sum

    val oldSetting = v.setting
    val oldScore = score

    var sampled = false

    /*val sampledFromEdge = v.node.edges.sortBy(_ => Math.random()).find { e =>
      try {
        e.f.potential.proposeSetting(e)
        true
      } catch {
        case e:CantProposeException => false
      }
    }

    sampledFromEdge match {
      case None =>
        println("Could not propose for " + v.label + "; Invoking sampleUniform")*/
        sampleUniform(v)
      /*case Some(e) =>
    }*/
    val newScore = score

    val accept = exp(newScore - oldScore) >= Math.random()
    if(fg.visualizationSamples.length < 500) fg.visualizationSamples.+=((v, v.value, accept))

    if(!accept) v.setting = oldSetting


  }


  def sampleUniform(v:Var[_]): Unit = v match {
    case v:DiscreteVar[_] => v.setting = Random.nextInt(v.dim)
    case v:ContinuousVar => v.setting = 42
  }

}
