package ml.wolfe.util

import cc.factorie.la.{SparseTensor1, DenseTensor1, DenseTensor2}
import org.scalatest.{Matchers, WordSpec}

/**
 * @author rockt
 */
class PimpMyFactorieSpecs extends WordSpec with Matchers {
  import PimpMyFactorie._

  "A pimped matrix" should {
    "be transposable" in {
      val A = new DenseTensor2(Seq(Seq(1.5, 1.0, 2.0), Seq(1.0, 5.0, 2.0)))
      val At = A.t

      (0 until A.dim1).foreach(row => (0 until A.dim2).foreach(col => A(row, col) shouldEqual At(col, row)))

      val y = new DenseTensor1(Array(2.0, 1.0))
      val x = new SparseTensor1(2)
      x.update(0, 2.0)
      x.update(1, 1.0)

      Seq(x,y).foreach(v => {
        val Atv = At * v
        val gold = new DenseTensor1(Array(4.0, 7.0, 6.0))
        Atv.toArray shouldEqual gold.toArray
      })
    }
  }
}
