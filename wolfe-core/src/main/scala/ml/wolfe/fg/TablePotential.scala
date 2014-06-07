package ml.wolfe.fg

import ml.wolfe.FactorGraph._
import scalaxy.loops._
import ml.wolfe.MoreArrayOps._
import ml.wolfe.FactorieVector


/**
 * @author Sebastian Riedel
 */
object TablePotential {

  def table(dims: Array[Int], pot: Array[Int] => Double) = {
    val count = dims.product
    val settings = Array.ofDim[Array[Int]](count)
    val scores = Array.ofDim[Double](count)
    for (i <- (0 until count).optimized) {
      val setting = TablePotential.entryToSetting(i, dims)
      val score = pot(setting)
      settings(i) = setting
      scores(i) = score
    }
    Table(settings, scores)
  }

  def apply(edges: Array[Edge], pot: Array[Int] => Double) = {
    val dims = edges.map(_.n.variable.asDiscrete.dim)
    new TablePotential(edges, table(dims, pot))
  }
  def apply(edges: Array[Edge], table: Table) = {
    new TablePotential(edges, table)
  }

  /**
   * Turns a setting vector into an entry number.
   * @param setting setting
   * @param dims dimensions of each variable.
   * @return the entry corresponding to the given setting.
   */
  final def settingToEntry(setting: Array[Int], dims: Array[Int]) = {
    var result = 0
    for (i <- (0 until dims.length).optimized) {
      result = setting(dims.length - i - 1) + result * dims(dims.length - i - 1)
    }
    result
  }

  /**
   * Turns an entry into a setting
   * @param entry the entry number.
   * @param dims dimensions of the variables.
   * @return a setting array corresponding to the entry.
   */
  final def entryToSetting(entry: Int, dims: Array[Int]) = {
    val result = Array.ofDim[Int](dims.length)
    var current = entry
    for (i <- (0 until dims.length).optimized) {
      val value = current % dims(i)
      result(i) = value
      current = current / dims(i)
    }
    result
  }


}

case class Table(settings: Array[Array[Int]], scores: Array[Double])

final class TablePotential(edges: Array[Edge], table: Table) extends Potential {

  import table._

  val dims       = edges.map(_.n.variable.asDiscrete.dim)
  val entryCount = table.scores.size

  lazy val vars = edges.map(_.n.variable.asDiscrete)

  /**
   * More verbose string representation that shows that potential table depending on factor type.
   * @param fgPrinter a printer that can print nodes and factors.
   * @return A verbose string representation of this factor.
   */
  def toVerboseString(implicit fgPrinter: FGPrinter) = {

    val tableString =
      for ((setting, index) <- settings.zipWithIndex) yield
        s"${ setting.mkString(" ") } | ${ table.scores(index) }"

    tableString.mkString("\n")
  }


  def valueForCurrentSetting() = {
    val setting = vars.map(_.setting)
    val entry = TablePotential.settingToEntry(setting, dims)
    scores(entry)
  }

  def penalizedScore(settingId: Int, setting: Array[Int]): Double = {
    var score = scores(settingId)
    for (j <- 0 until edges.size) {
      score += edges(j).n2f(setting(j))
    }
    score
  }

  def maxMarginalF2N(edge: Edge) = {
    //max over all settings
    fill(edge.f2n, Double.NegativeInfinity)

    for (i <- 0 until settings.size) {
      val setting = settings(i)
      var score = scores(i)
      val varValue = setting(edge.indexInFactor)
      for (j <- 0 until edges.size; if j != edge.indexInFactor) {
        score += edges(j).n2f(setting(j))
      }
      edge.f2n(varValue) = math.max(score, edge.f2n(varValue))
    }
    maxNormalize(edge.f2n)

  }
  def maxMarginalExpectationsAndObjective(result: FactorieVector) = {
    // 1) go over all states, find max with respect to incoming messages
    var norm = Double.NegativeInfinity
    var maxScore = Double.NegativeInfinity
    for (i <- (0 until entryCount).optimized) {
      val setting = settings(i)
      val score = penalizedScore(i, setting)
      if (score > norm) {
        norm = score
        maxScore = scores(i)
      }
    }


    maxScore
  }
}