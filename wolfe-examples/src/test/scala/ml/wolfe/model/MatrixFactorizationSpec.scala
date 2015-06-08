package ml.wolfe.model

import ml.wolfe.Vect
import ml.wolfe.term._
import ml.wolfe.term.TermImplicits._
import ml.wolfe.term.Transformer._
import ml.wolfe.util.Math._
import org.scalatest.{Matchers, WordSpec}

/**
 * @author rockt
 */
class MatrixFactorizationSpec extends WordSpec with Matchers {
  val eps = 0.01
  "A matrix factorization model" should {
    "recover a data matrix when using sufficient dimension k" in {
      val observed = Seq(0 -> 0, 0 -> 1, 1 -> 1, 2 -> 0)
      val unobserved = Seq(1 -> 0, 2 -> 1)
      val mf = MatrixFactorization.train(observed, kParam = 100, epochsParam = 100, lambdaParam = 0.0)
      print(" " * 5)
      (0 until mf.numCols).foreach(col => print("%4d ".format(col)))
      println()
      (0 until mf.numRows).foreach(row => {
        print("%4d ".format(row))
        (0 until mf.numCols).foreach(col => {
          print("%1.2f ".format(mf.predict(row, col)))
        })
        println()
      })
      observed.foreach(t => mf.predict(t._1, t._2) should be(1.0 +- eps))
      unobserved.foreach(t => mf.predict(t._1, t._2) should be(0.0 +- eps))
    }
  }
}
