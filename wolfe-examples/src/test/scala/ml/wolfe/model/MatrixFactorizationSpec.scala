package ml.wolfe.model

import org.scalatest.{Matchers, WordSpec}

/**
 * @author rockt
 */
class MatrixFactorizationSpec extends WordSpec with Matchers {
  val eps = 0.05
  "A matrix factorization model" should {
    "optimize a log likelihood loss" ignore {
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
