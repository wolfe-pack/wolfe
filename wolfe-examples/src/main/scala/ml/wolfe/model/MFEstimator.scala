package ml.wolfe.model

import cc.factorie.la.DenseTensor2
import ml.wolfe._
import ml.wolfe.util.Math._

/**
 * @author riedelcastro
 */
object MFEstimator {

  import ml.wolfe.term.TermImplicits._
  import ml.wolfe.term._

  @domain case class Factorization(a: IndexedSeq[Vect], v: IndexedSeq[Vect])

  def learn(mat: Mat, k: Int = 2, 
            l2Reg: Double = 0.1, l1Reg:Double = 0.1,
            epochs:Int = 10, logistic: Boolean = false): Factorization = {
    val n = mat.dim1
    val m = mat.dim2
    val Thetas = Factorization.Values(Seqs(Vectors(k), n), Seqs(Vectors(k), m))
    val M = Matrices(n,m)
    val data = M.Const(mat) //this is only necessary because intellij goes crazy
    implicit val random = new scala.util.Random(0)
    val init = Settings(Thetas.createRandomSetting(random.nextGaussian() * 0.1))
    val adaParams = AdaGradParameters(epochs = epochs, learningRate = 0.1, initParams = init)

    def score(theta: Thetas.Term)(row: IntTerm, col: IntTerm) = {
      theta.a(row) dot theta.v(col)
    }

    def l2Obj(data: M.Term)(t: Thetas.Term) = {
      sum(0 until intToConstant(n)) { row =>
        sum(0 until intToConstant(n)) { col =>
          -sq(score(t)(row, col) - data(row,col)) - l2Reg * (l2(t.a(row)) + l2(t.v(col)))
        }
      }
    }
    val result = argmax(Thetas)(l2Obj(data)) by Argmaxer.adaGrad(adaParams)
    result.eval()
  }

  def main(args: Array[String]) {
    val n = 5
    val data = new DenseTensor2(n,n)
    for (i <- 0 until n; j <- 0 until n) data(i,j) = random.nextGaussian()
    
    val result = learn(data, k = 2, epochs = 100, l2Reg = 0.01)
    println(result)
    
  }
}