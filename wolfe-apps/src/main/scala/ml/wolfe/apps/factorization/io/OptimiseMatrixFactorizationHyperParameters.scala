package ml.wolfe.apps.factorization.io

import java.io.{FileWriter, File}

import cc.factorie.optimize._
import ml.wolfe.GradientBasedOptimizer
import ml.wolfe.apps.factorization._
import ml.wolfe.fg.{L2Regularization, CellLogisticLoss, VectorMsgs}
import ml.wolfe.util._
import ml.wolfe.{DenseVector, GradientBasedOptimizer, Wolfe}

import scala.io.Source
import scala.util.Random


/**
 * Created by Ingolf on 06/11/2014.
 */
object OptimiseMatrixFactorizationHyperParameters extends App {
  val mfp = new MatrixFactorisationProblem()

  val myOptimizer: HyperParameterOptimisationAlgorithm = new NelderMeadSimplex()
  myOptimizer.optimise(mfp, Map[String, Double]("mf.lambda" -> 0.01, "mf.alpha" -> 0.1))

  println("Best wMAP: " + myOptimizer.bestScore)
  println("Best parameters:\n" + myOptimizer.bestParameters)
}

class MatrixFactorisationProblem extends OptimisationProblem {
  override val parametersToOptimize: Seq[HyperParameter] = Seq(HyperParameter("mf.lambda"), HyperParameter("mf.alpha"))
  val startingValues = Map[String, Double]("mf.lambda" -> 0.01, "mf.alpha" -> 0.1)

  /**
   * Evaluate the optimisation problem given the set of hyper parameters.
   * @param hyperparameters The map of hyper parameters
   * @return The score of this evaluation, higher is better
   */
  override def evaluate(hyperparameters: Map[String, Double]): Double = {
    val confPath = "conf/mf-debug.conf"
    val newConfPath = "conf/mf-hyper.conf"

    OverrideConfig(hyperparameters, newConfPath, confPath)

    val mf = new MatrixFactorization(newConfPath)

    -mf.run()
  }
}
