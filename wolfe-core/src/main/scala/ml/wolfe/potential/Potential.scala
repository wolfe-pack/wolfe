package ml.wolfe.potential

import ml.wolfe.FactorGraph._
import scalaxy.loops._
import ml.wolfe.FactorGraph


/**
 * @author Sebastian Riedel
 */
trait Potential {


  def maxMarginalF2N(edge:Edge)
  //def updateValue()

}

final class TablePotential(edges:Array[Edge],settings:Array[Array[Int]],scores:Array[Double]) extends Potential {
  def maxMarginalF2N(edge: Edge) = {
    //max over all settings
    for (i <- (0 until settings.size).optimized) {
      val setting = settings(i)
      var score = scores(i)
      val varValue = setting(edge.indexInFactor)
      for (j <- (0 until edges.size).optimized; if j != edge.indexInFactor) {
        score += edges(j).n2f(setting(j))
      }
      edge.f2n(varValue) = math.max(score, edge.f2n(varValue))
    }
  }
}

object TablePotential {
  def apply(edges:Array[Edge],pot:Array[Int] => Double) = {
    val dims = edges.map(_.n.dim)
    val count = dims.product
    val settings = Array.ofDim[Array[Int]](count)
    val scores = Array.ofDim[Double](count)
    for (i <- (0 until count).optimized) {
      val setting = FactorGraph.entryToSetting(i,dims)
      val score = pot(setting)
      settings(i) = setting
      scores(i) = score
    }
    new TablePotential(edges,settings,scores)
  }
}

