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
/*
    val tableString =
      for ((setting, index) <- settings.zipWithIndex) yield
        s"${
          setting.zipWithIndex.map({case (s,j) => vars(j).domainLabels(s)}).mkString("\t")
        }\t| ${ table.scores(index) }"

    tableString.mkString("\n")*/

    val headerRow = "<tr>" + vars.map(_.label).map("<td><i>" + _ + "</i></td>").mkString(" ") + "</tr>"
    val tableRows =
      for ((setting, index) <- settings.zipWithIndex) yield {
        val domainEntries = for ((s, j) <- setting.zipWithIndex) yield vars(j).domainLabels(s)
        val cells = domainEntries :+ ("<b>" + table.scores(index).toString + "</b>")
        "<tr>" + cells.map("<td>" + _ + "</td>").mkString(" ") + "</tr>"
      }

    "<table class='potentialtable'>" + headerRow + "\n" + tableRows.mkString("\n") + "</table>"
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
      fill(msgs(j).f2n, 0)

    val maxSetting = computeMAP()

    for (j <- (0 until edges.size).optimized)
      msgs(j).f2n(maxSetting(j)) = 1
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






  //todo: make this less ugly and extract generic functionality to a different class
  var Wt: ArrayBuffer[Int] = null
  var vt: breeze.linalg.DenseVector[Double] = null
  var warmStart = false
  override def quadraticProgramF2N(stepSize:Double, maxIterations:Int) : Unit = {
    import breeze.linalg._
    println(s"quadraticProgramF2N($stepSize, $maxIterations)")

    //Initialise the active set to only contain the MAP setting
    if(!warmStart) {
      Wt = ArrayBuffer(TablePotential.settingToEntry(computeMAP(), dims))
      vt = DenseVector.zeros[Double](settings.length)
      vt(Wt(0)) = 1
      warmStart = true
    }

    val Ms : Array[DenseMatrix[Double]] = vars.map(v => DenseMatrix.zeros[Double](v.dim, settings.length))
    for(i <- 0 until settings.length) {
      val setting = TablePotential.entryToSetting(i, dims)
      for(j <- 0 until vars.length) {
        Ms(j)(setting(j), i) = 1
      }
    }
    println(s"Ms=\n${Ms.mkString("\n")}")


    // M' a
    val foo = DenseVector.zeros[Double](settings.length)
    for (k <- (0 until edges.size).optimized) {
      for (i <- (0 until settings.length).optimized) {
        foo(i) += vars(k).b(settings(i)(k))
        foo(i) += msgs(k).n2f(settings(i)(k)) / stepSize
      }
    }
    println(s"M'a:$foo")

    var solved = false
    for(t <- 0 until maxIterations if !solved) {
      println(s"\n\nt=$t")
      println(s"v_t:$vt")
      println(s"W_t:${Wt.mkString("(", ",", ")")}")
      // ------------------- Solve the KKT system -----------------------------
      //todo: Don't need to re-invert every time. Initialize and then modify inverse as we go along

      val aaaaaaaa = DenseMatrix.zeros[Double](settings.length, settings.length)
      for (i <- 0 until settings.length;
           j <- 0 until settings.length;
           k <- 0 until edges.size
           if settings(i)(k) == settings(j)(k))
        aaaaaaaa(i, j) += 1

      // (M_J' . M_J, 1), (1, 0)
      val constraintsMatrix = DenseMatrix.zeros[Double](Wt.size + 1, Wt.size + 1)
      for (i <- (0 until Wt.length).optimized) {
        for (j <- (0 until Wt.length).optimized)
          for (k <- (0 until edges.size).optimized)
            if (settings(Wt(i))(k) == settings(Wt(j))(k)) constraintsMatrix(i, j) += 1
        constraintsMatrix(Wt.size, i) = 1
        constraintsMatrix(i, Wt.size) = 1
      }
      println(s"Constraints Matrix:\n$constraintsMatrix")

      val messagesVector = DenseVector.zeros[Double](Wt.size + 1)
      messagesVector(0 to Wt.size-1) := foo(Wt)
      for (i <- (0 until Wt.size).optimized) {
        val entry = TablePotential.settingToEntry(settings(Wt(i)), dims)
        messagesVector(i) += scores(entry) / stepSize
      }
      messagesVector(Wt.size) = 1
      println(s"Messages Vector:$messagesVector")

      val vAndTau = try {
        constraintsMatrix \ messagesVector
      } catch {
        case e:MatrixSingularException =>
          println("Singular!")
          val (eigenValuesRe, eigenValuesIm, eigenVectors) = eig(constraintsMatrix)
          (0 until eigenValuesRe.length) find (i => eigenValuesRe(i) == 0 && eigenValuesIm(i) == 0) match {
            case Some(j:Int) => eigenVectors(::, j)
          }
      }

      //----------------------------------------------------------------------
      def vecSlice(vec:DenseVector[Double], indices:Seq[Int]) = vec(indices.head, indices.tail :_*)
      val vj = DenseVector.zeros[Double](Wt.length)
      vecSlice(vj, 0 until Wt.length) := vAndTau(0 until vAndTau.length-1)
      val tau = vAndTau(-1)
      println(s"v:$vj on $Wt")
      println(s"tau:$tau")


      //m'w = m'a - m'mv
      val pen = foo.toDenseVector - aaaaaaaa(0 until settings.length, Wt) * vj


      var obj:Double = 0
      for(i <- 0 until vars.length) {
        val qia = Ms(i)(0 until vars(i).dim, Wt) * vj
        for(s <- 0 until vars(i).dim) {
          val x = qia(s) - vars(i).b(s) - msgs(i).n2f(s)
          obj += 0.5 * (x * x)
        }
      }
      for(i <- 0 until Wt.length) {
        obj -= scores(Wt(i)) * vj(i)
      }
      println(s"Subproblem Objective: $obj")

      if (vj == vt(Wt)) {
        println("Same v!")
        val map = computeMAP(i => scores(i) + pen(i))
        val r = TablePotential.settingToEntry(map, dims)
        if (scores(r) + pen(r) <= tau + 1e-12) {
          solved = true
          println("SOLVED")
        } else {
          println(s"Adding $r because ${scores(r)} + ${pen(r)} = ${scores(r) + pen(r)} > $tau")
          Wt.append(r)
        }
      } else {
        var alpha: Double = 1
        var blockingConstraint:Option[Int] = None
        for ((r,i) <- Wt.zipWithIndex if vt(r) > vj(i)) {
          val x = vt(r) / (vt(r) - vj(i))
          if (x < alpha) {
            alpha = x
            blockingConstraint = Some(r)
          }
        }

        for ((r,i) <- Wt.zipWithIndex) {
          vt(r) = vt(r) * (1 - alpha) + vj(i) * alpha
        }

        println(s"Blocking Constraint: $blockingConstraint")
        blockingConstraint match {
          case Some(r:Int) => Wt -= r
          case _ =>
        }

      }
    }

    for (j <- (0 until edges.size).optimized)
      fill(msgs(j).f2n, 0)

    for(r <- Wt) {
      val setting = settings(r)
      for (j <- (0 until edges.size).optimized)
        msgs(j).f2n(setting(j)) += vt(r)
    }
  }
}