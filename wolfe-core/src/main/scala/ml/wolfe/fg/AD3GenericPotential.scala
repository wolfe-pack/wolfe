package ml.wolfe.fg

import breeze.linalg._
import ml.wolfe.MoreArrayOps._
import scalaxy.loops._

import scala.collection.mutable.ArrayBuffer

/**
 * Created by luke on 15/08/14.
 */
trait AD3GenericPotential extends DiscretePotential {
  def settings:Array[Array[Int]]
  /* MAP with N2F penalties */
  def computeMAP() : Array[Int]

  /* MAP of arbitrary function */
  def computeMAP(scoreFun : Int => Double) : Array[Int]

  def getScores: Array[Double] = getScoreTable.array
  private var scores:Array[Double] = null
  private var activeSet: ArrayBuffer[Int] = null
  private var solution: DenseVector[Double] = null
  private var AInv : DenseMatrix[Double] = null // Inverse of [[M'M 1], [1' 0]], where M is indexed by activeSet
  private var ANull : DenseVector[Double] = null // Any vector in the null space of A
  private lazy val consistencyMatrices : Array[DenseMatrix[Double]] = {
    val Ms = vars.map(v => DenseMatrix.zeros[Double](v.dim, settings.length))
    for (i <- 0 until settings.length) {
      val setting = TablePotential.entryToSetting(i, dims)
      for (j <- 0 until vars.length) Ms(j)(setting(j), i) = 1
    }
    Ms
  }

  override def ad3Init(): Unit = {
    activeSet = ArrayBuffer(TablePotential.settingToEntry(computeMAP(), dims))
    solution = DenseVector.zeros[Double](settings.length)
    solution(activeSet(0)) = 1
    AInv = DenseMatrix((0d, 1d), (1d, -vars.length.toDouble))
    scores = getScores
  }

  override def quadraticProgramF2N(stepSize:Double, maxIterations:Int) : Unit = {
    // todo: cache solution (see page 15)
    // M'a
    val Mta = DenseVector.zeros[Double](settings.length)
    for (k <- (0 until vars.size).optimized) {
      for (i <- (0 until settings.length).optimized) {
        Mta(i) += vars(k).b(settings(i)(k))
        Mta(i) += msgss(k).n2f(settings(i)(k)) / stepSize
      }
    }

    /* Main Loop */
    var solved = false
    for(t <- 0 until maxIterations if !solved) {
      // ------------------- Solve the KKT system -----------------------------
      val vAndTau = if(AInv == null) {
        ANull
      } else {
        val rhsKKT = DenseVector.vertcat(Mta(activeSet).toDenseVector, DenseVector.ones[Double](1))
        for (i <- (0 until activeSet.size).optimized) {
          val entry = TablePotential.settingToEntry(settings(activeSet(i)), dims)
          rhsKKT(i) += scores(entry) / stepSize
        }
        AInv * rhsKKT
      }

      val vj:DenseVector[Double] = vAndTau(0 until vAndTau.length-1) // new solution, indexed by activeSet
      val tau = vAndTau(-1)

      //----------------------------------------------------------------------

      //M' * (M_J)
      val MtMj = DenseMatrix.tabulate[Double](settings.length, activeSet.length){ case(i, j) =>
        (0 until vars.length) count (k => settings(i)(k) == settings(activeSet(j))(k)) }
      //M'w
      val Mtw = Mta.toDenseVector - MtMj * vj

      if (vj == solution(activeSet)) {

        val map = computeMAP(i => scores(i) + Mtw(i))
        val r = TablePotential.settingToEntry(map, dims)
        if (scores(r) + Mtw(r) <= tau + 1e-12) solved = true
        else addToActiveSet(r)

      } else {

        var alpha: Double = 1
        var blockingConstraint:Option[Int] = None
        for ((r,i) <- activeSet.zipWithIndex if solution(r) > vj(i)) {
          val x = solution(r) / (solution(r) - vj(i))
          if (x < alpha) {
            alpha = x
            blockingConstraint = Some(r)
          }
        }
        for ((r,i) <- activeSet.zipWithIndex) solution(r) = solution(r) * (1 - alpha) + vj(i) * alpha

        blockingConstraint match {
          case Some(r:Int) => removeFromActiveSet(r)
          case _ => }

      }

    }

    for (j <- (0 until msgss.size).optimized)
      fill(msgss(j).f2n, 0)

    for(r <- activeSet) {
      val setting = settings(r)
      for (j <- (0 until msgss.size).optimized)
        msgss(j).f2n(setting(j)) += solution(r)
    }
  }


  private def addToActiveSet(r:Int): Unit = {
    if(AInv == null) {
      activeSet += r
      setAInverse()
    } else {
      AInvInsert(r)
      activeSet += r
    }
    if(AInv == null) setANull()
  }

  private def removeFromActiveSet(r:Int): Unit = {
    if(AInv == null) {
      activeSet -= r
      setAInverse()
    } else {
      AInvRemove(r)
      activeSet -= r
    }
    if(AInv == null) setANull()
  }

  private def AInvInsert(r:Int) = {
    /*   Update A⁻¹ = (MjMj')⁻¹ by blockwise inversion   */
    val B = DenseVector.tabulate[Double](activeSet.length + 1){ case(i) =>
      if(i < activeSet.length) (0 until vars.length) count { k => settings(r)(k) == settings(activeSet(i))(k) }
      else 1
    }

    val C = B.t
    val d = vars.length

    val P:Transpose[DenseVector[Double]] = C * AInv  // CA⁻¹
    val Q:DenseVector[Double] = AInv * B             // A⁻¹B
    val p:Double = d - (P * B)                // (D - CA⁻¹B)⁻¹
    if(Math.abs(p) < 1e-9) {
      //A is Singular
      AInv = null
    } else {
      ANull = null
      val q = 1d / p
      val R:DenseVector[Double] = Q * q                 // A⁻¹B(D - CA⁻¹B)⁻¹

      val X1:DenseMatrix[Double] = AInv + (R * P)      // A⁻¹ + A⁻¹B(D - CA⁻¹B)⁻¹CA⁻¹
      val X2:DenseVector[Double] = R * (-1d)             // -A⁻¹B(D - CA⁻¹B)⁻¹
      val X3:Transpose[DenseVector[Double]] = P * (-q)  // -CA⁻¹(D - CA⁻¹B)⁻¹


      AInv = DenseMatrix.vertcat(
        DenseMatrix.horzcat(X1, X2.toDenseMatrix.t),
        DenseMatrix.horzcat(X3.t.toDenseMatrix, new DenseMatrix[Double](1, 1, Array(q)))
      )
      // At this point the matrix basis is out of order - we need to swap the final two dimensions
      // (corresponding to tau and v(r), respectively)
      val n = activeSet.length
      for(i <- (0 until n+2).optimized) { val x = AInv(i, n+1); AInv(i, n+1) = AInv(i, n); AInv(i, n) = x }
      for(i <- (0 until n+2).optimized) { val x = AInv(n+1, i); AInv(n+1, i) = AInv(n, i); AInv(n, i) = x }
    }

  }

  private def AInvRemove(r:Int) = {

    /*   Update A⁻¹ by http://math.stackexchange.com/questions/208001/are-there-any-decompositions-of-a-symmetric-matrix-that-allow-for-the-inversion/208021#208021*/
    val n = activeSet.length
    val s = activeSet.indexOf(r)
    val B = CSCMatrix.Builder

    // Permutation
    for(i <- (0 until n+1).optimized) {
      val x = AInv(i, s)
      for(j <- (s until n).optimized) AInv(i, j) = AInv(i, j+1)
      AInv(i, n) = x
    }
    for(i <- (0 until n+1).optimized) {
      val x = AInv(s, i)
      for(j <- (s until n).optimized) AInv(j, i) = AInv(j+1, i)
      AInv(n, i) = x
    }

    val E = AInv(0 until n, 0 until n)
    val f:DenseVector[Double] = AInv(0 until n, n)
    val g:DenseVector[Double] = AInv(n, 0 until n).t
    val h = AInv(n, n)
    if(Math.abs(h) < 1e-9) {
      //A is Singular
      AInv = null
    } else {
      AInv = E - (f * g.t) / h
    }
  }

  def dualObjective(stepSize:Double) = {
    def sq(x:Double) = x * x
    var x:Double = 0
    var y:Double = 0
    var z:Double = 0
    var w:Double = 0
    for(r <- activeSet) {
      x += solution(r) * scores(r)
      for(s <- 0 until vars.length) {
        y += solution(r) * factor.edges(s).msgs.asDiscrete.n2f(settings(r)(s))
      }
    }
    for(s <- 0 until vars.length) {
      for(t <- 0 until vars(s).asDiscrete.dim) {
        z -= factor.edges(s).msgs.asDiscrete.n2f(t) * vars(s).asDiscrete.b(t)
        w -= (stepSize / 2) * sq(factor.edges(s).msgs.asDiscrete.f2n(t) - vars(s).asDiscrete.b(t))
      }
    }
    //println(s"$x + $y + $z + $w = ${x+y+z+w}")
    x + y + z + w
  }

  private def getA():DenseMatrix[Double] = {
    val A = DenseMatrix.zeros[Double](activeSet.size + 1, activeSet.size + 1)
    for (i <- (0 until activeSet.length).optimized) {
      for (j <- (0 until activeSet.length).optimized)
        for (k <- (0 until vars.size).optimized)
          if (settings(activeSet(i))(k) == settings(activeSet(j))(k)) A(i, j) += 1
      A(activeSet.size, i) = 1
      A(i, activeSet.size) = 1
    }
    A
  }

  private def setAInverse(): Unit = {
    try {
      AInv = inv(getA())
    } catch {
      case _:MatrixSingularException => AInv = null
    }
  }

  private def setANull(): Unit = {
    val A = getA()

    val (eigenValuesRe, eigenValuesIm, eigenVectors) = eig(A)
    val i: Int = (0 until eigenValuesRe.length).find(i => Math.abs(eigenValuesRe(i)) < 1e-9 && Math.abs(eigenValuesIm(i)) < 1e-9).get
    ANull = eigenVectors(::, i)
  }
}
