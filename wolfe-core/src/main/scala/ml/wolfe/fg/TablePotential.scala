package ml.wolfe.fg

import ml.wolfe.FactorGraph._
import ml.wolfe.util.Multidimensional._
import scala.collection.mutable.ArrayBuffer
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


  //todo: move these both into Util, to share code with LabelledTensor
  /**
   * Turns a setting vector into an entry number.
   * @param setting setting
   * @param dims dimensions of each variable.
   * @return the entry corresponding to the given setting.
   */
  final def settingToEntry(setting: Array[Int], dims: Array[Int]) = {
    var result = 0
    for (i <- (0 until dims.length).optimized) {
      result = setting(i) + result * dims(i)
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
      val value = current % dims(dims.length - i - 1)
      result(dims.length - i - 1) = value
      current = current / dims(dims.length - i - 1)
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
  lazy val msgs = edges.map(_.msgs.asDiscrete)

  /**
   * More verbose string representation that shows that potential table depending on factor type.
   * @param fgPrinter a printer that can print nodes and factors.
   * @return A verbose string representation of this factor.
   */
  override def toVerboseString(implicit fgPrinter: FGPrinter) = {

    val tableString =
      for ((setting, index) <- settings.zipWithIndex) yield
        s"${ setting.mkString(" ") } | ${ table.scores(index) }"

    tableString.mkString("\n")
  }

  override def getScoreTable(forVariables:Array[DiscreteVar]) : LabelledTensor[DiscreteVar, Double] = {
    def curr = LabelledTensor.onExistingArray[DiscreteVar, Double](vars, _.dim, table.scores)
    curr.permute(forVariables, allowSameArray = true)
  }

  override def valueForCurrentSetting() = {
    val setting = vars.map(_.setting)
    val entry = TablePotential.settingToEntry(setting, dims)
    scores(entry)
  }

  def penalizedScore(settingId: Int, setting: Array[Int]): Double = {
    var score = scores(settingId)
    for (j <- 0 until msgs.size) {
      score += msgs(j).n2f(setting(j))
    }
    score
  }

  override def maxMarginalF2N(edge: Edge) = {
    //max over all settings
    val m = edge.msgs.asDiscrete
    fill(m.f2n, Double.NegativeInfinity)

    for (i <- 0 until settings.size) {
      val setting = settings(i)
      var score = scores(i)
      val varValue = setting(edge.indexInFactor)
      for (j <- 0 until edges.size; if j != edge.indexInFactor) {
        score += msgs(j).n2f(setting(j))
      }
      m.f2n(varValue) = math.max(score, m.f2n(varValue))
    }
    maxNormalize(m.f2n)

  }


  override def marginalF2N(edge: Edge) = {
    val m = edge.msgs.asDiscrete
    fill(m.f2n, 0.0)

    for (i <- 0 until settings.size) {
      val setting = settings(i)
      var score = scores(i)
      val varValue = setting(edge.indexInFactor)
      for (j <- 0 until edges.size; if j != edge.indexInFactor) {
        score += msgs(j).n2f(setting(j))
      }
      m.f2n(varValue) = m.f2n(varValue) + math.exp(score)
    }
    //normalize
    normalize(m.f2n)
    //convert to log space
    log(m.f2n)

  }

  def penalizedScore(i:Int) = scores(i) + (0 until edges.size).map(j => msgs(j).n2f(settings(i)(j))).sum

  private def computeMAP(scoreFun : Int => Double = penalizedScore) = {
    var maxScore = Double.NegativeInfinity
    var maxSetting = Array.ofDim[Int](edges.size)

    for (i <- (0 until settings.size).optimized) {
      val setting = settings(i)
      var score = scoreFun(i)
      /*for (j <- (0 until edges.size).optimized)
        score += msgs(j).n2f(setting(j))*/

      if(score > maxScore) {
        maxScore = score
        maxSetting = setting
      }
    }

    maxSetting
  }

  override def mapF2N() = {
    for (j <- (0 until edges.size).optimized)
      fill(msgs(j).f2n, Double.NegativeInfinity)

    val maxSetting = computeMAP()

    for (j <- (0 until edges.size).optimized)
      msgs(j).f2n(maxSetting(j)) = 0
  }

  override def maxMarginalExpectationsAndObjective(result: FactorieVector) = {
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

  override def marginalExpectationsAndObjective(dstExpectations: FactorieVector) = {
    var localZ = 0.0

    //calculate local partition function
    for (i <- (0 until entryCount).optimized) {
      val setting = settings(i)
      val score = penalizedScore(i, setting)
      localZ += math.exp(score)
    }

    var linear = 0.0
    var entropy = 0.0
    //calculate linear contribution to objective and entropy
    for (i <- (0 until entryCount).optimized) {
      val setting = settings(i)
      val score = penalizedScore(i, setting)
      val prob = math.exp(score) / localZ
      linear += scores(i) * prob
      entropy -= math.log(prob) * prob
    }
    val obj = linear + entropy
    obj
  }




  override def quadraticProgramF2N(stepSize:Double, maxIterations:Int) : Unit = {
    println(s"quadraticProgramF2N($stepSize, $maxIterations)")

    import breeze.linalg._

    //Initialise the active set to only contain the MAP setting
    var activeSet: ArrayBuffer[Int] = ArrayBuffer(TablePotential.settingToEntry(computeMAP(), dims))
    var vEst = DenseVector.zeros[Double](settings.length)
    vEst(activeSet(0))=1
    var activeSetLast = activeSet

    var solved = false
    var t = 0
    while(t<maxIterations && !solved) {
      t += 1
      println(s"\n\nIteration $t")
      println("Active Set:" + activeSet.mkString("(", ",", ")"))
      // ------------------- Solve the KKT system -----------------------------
      //todo: Don't need to do this every time. Initialize and then modify inverse as we go along
      // (M_J' . M_J, 1), (1, 0)
      val constraintsMatrix = DenseMatrix.zeros[Double](activeSet.size + 1, activeSet.size + 1)
      for (i <- (0 until activeSet.size).optimized) {
        for (j <- (0 until activeSet.size).optimized)
          for (k <- (0 until edges.size).optimized)
            if (settings(activeSet(i))(k) == settings(activeSet(j))(k)) constraintsMatrix(i, j) += 1
        constraintsMatrix(activeSet.size, i) = 1
        constraintsMatrix(i, activeSet.size) = 1
      }
      println(s"Constraints Matrix:\n$constraintsMatrix")

      // M' a
      val foo = DenseVector.zeros[Double](settings.size)
      for (i <- (0 until settings.length).optimized) {
        for (k <- (0 until edges.size).optimized) {
          foo(i) += math.exp(edges(k).n.variable.asDiscrete.b(settings(i)(k)))
          foo(i) += edges(k).msgs.asDiscrete.n2f(settings(i)(k)) / stepSize
        }
      }
      println(s"M'a:$foo")

      val messagesVector = DenseVector.zeros[Double](activeSet.size + 1)
      messagesVector(1 to activeSet.size) := foo(activeSet)
      for (i <- (0 until activeSet.size).optimized) {
        val entry = TablePotential.settingToEntry(settings(activeSet(i)), dims)
        messagesVector(i) += scores(entry)
      }
      messagesVector(activeSet.size) = 1
      println(s"Messages Vector:$messagesVector")

      val vt = constraintsMatrix \ messagesVector
      println(s"vt:$vt")
      //----------------------------------------------------------------------
      def vecSlice(vec:DenseVector[Double], indices:Seq[Int]) = vec(indices.head, indices.tail :_*)

      val v = DenseVector.zeros[Double](settings.length)
      vecSlice(v, activeSet) := vt(0 until vt.length - 1)
      val tau = vt(-1)

      val pen = foo.toDenseVector
      val bar = constraintsMatrix(0 until activeSet.size, 0 until activeSet.size)
      val x = bar * v(activeSet)
      pen(activeSet) := pen(activeSet) - x

      if (activeSetLast.view == activeSet.view && vEst(activeSetLast) == v(activeSet)) {
        println("Same v as previous")
        val map = computeMAP(i => scores(i) + pen(i))
        val r = TablePotential.settingToEntry(map, dims)
        if (scores(r) + pen(r) <= tau) {
          solved = true
          println("SOLVED")
        } else
          activeSet.append(r)
      } else {
        var alpha: Double = 1
        for (r <- activeSetLast) {
          if (vEst(r) > v(r))
            alpha = min(alpha, vEst(r) / (vEst(r) - v(r)))
          vecSlice(vEst, activeSetLast) := 0.0
          vecSlice(vEst, activeSet) := ( vecSlice(vEst,activeSet) :* (1 - alpha) )  :+   ( vecSlice(v,activeSet) :* alpha )
        }

        var foundBlockingConstraint = false
        for(r <- activeSet if !foundBlockingConstraint && scores(r) + pen(r) <= tau) {
          foundBlockingConstraint = true
          activeSet = activeSet - r
        }
      }
      activeSetLast = activeSet
    }

    for (j <- (0 until edges.size).optimized)
      fill(msgs(j).f2n, 0)

    for(i <- (0 until settings.length).optimized) {
      val setting = settings(i)
      for (j <- (0 until edges.size).optimized)
        msgs(j).f2n(setting(j)) += vEst(i)
    }

    for (j <- (0 until edges.size).optimized)
      log(msgs(j).f2n)
  }
}