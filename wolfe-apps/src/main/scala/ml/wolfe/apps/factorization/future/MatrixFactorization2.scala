package ml.wolfe.apps.factorization.future

import cc.factorie.la.DenseTensor1
import cc.factorie.optimize.{AdaGrad, BatchTrainer}
import ml.wolfe.fg20._
import ml.wolfe.term.Setting
import ml.wolfe.util.Math._

import scala.util.Random

/**
 * @author rockt
 */
object MatrixFactorization2 extends App {
  val k = 5
  val cols = Array("r1", "r2", "r3", "r4", "r5").map(new VectVar(k, _))
  val rows = Array("e1", "e2", "e3", "e4", "e5").map(new VectVar(k, _))

  val initialState = new MapBasedState(
    (cols ++ rows).map(_ -> new DenseTensor1((0 until k).map(i => random.nextGaussian() * 0.1).toArray)).toMap
  )

  val data = Array(
    Array(1, 0, 1, 0, 1),
    Array(1, 1, 0, 0, 1),
    Array(0, 0, 0, 1, 0),
    Array(1, 0, 1, 0, 0),
    Array(1, 0, 0, 0, 1)
  )

  val potentials =
    (0 until rows.length).flatMap(r => {
      (0 until cols.length).collect {
        case c if data(r)(c) == 1 =>
          new FlatSum[Differentiable](Seq(
            new MFLogisticPotential(rows(r), cols(c)),
            new L2Regularization(0.01, rows(r), cols(c))
          )) with DifferentiableSum
      }
    })

  val stochasticPotentials =
    (0 until rows.length).flatMap(r => {
      (0 until cols.length).collect {
        case c if data(r)(c) == 1 =>
          def sampledRow = rows(random.nextInt(rows.length))

          /*
          new FlatSum[Differentiable](Seq(
            new MFLogisticPotential(sampledRow, cols(c), 0.0),
            new L2Regularization(0.01, sampledRow, cols(c))
          )) with DifferentiableSum
          */

          new MFLogisticPotential(sampledRow, cols(c), 0.0)
      }
    })

  val problem = Problem(potentials ++ stochasticPotentials)

  val optimizer = new GradientBasedOptimizer(problem)

  val result = optimizer.gradientBasedArgmax(new BatchTrainer(_, new AdaGrad(0.1), 100), init = initialState)


  print("\t")
  cols.foreach(c => print(c + "\t"))
  println()
  rows.foreach(r => {
    print(r.name + "\t")
    cols.foreach(c => print(sigmoid(result.state(r) dot result.state(c)) + "\t"))
    println()
  })

}



class MFLogisticPotential(rowVar: => VectVar, colVar: => VectVar, target: Double = 1.0)
  extends StatelessDifferentiable with StatelessScorer with VectPotential {

  override def vectVars: Array[VectVar] = Array(rowVar, colVar)

  private def innerLossAndDirection(s: Double): (Double, Int) =
    if (target >= s) (1 + s - target, 1)
    else (1 + target - s, -1)


  override def score(setting: Setting): Double = {
    val row = setting.vect(0)
    val col = setting.vect(1)

    val score = sigmoid(row dot col)

    val (loss, dir) = innerLossAndDirection(score)

    math.log(loss)
  }

  override def gradientAndValue(currentParameters: PartialSetting, gradient: Setting): Double = {
    val row = currentParameters.vect(0)
    val col = currentParameters.vect(1)

    val score = sigmoid(row dot col)

    val (loss, dir) = innerLossAndDirection(score)

    gradient.vect(0) = col * (1.0 - loss) * dir
    gradient.vect(1) = row * (1.0 - loss) * dir

    math.log(loss)
  }
}

/**
 * λ * Σ_i ||v_i||²
 */
class L2Regularization(lambda: Double, vars: VectVar*) extends StatelessDifferentiable with StatelessScorer with VectPotential {
  override def vectVars: Array[VectVar] = vars.toArray

  override def score(setting: Setting): Double =
    if (lambda == 0) 0
    else -lambda * setting.vect.map(v => v.twoNormSquared).sum

  override def gradientAndValue(currentParameters: PartialSetting, gradient: Setting): Double = {
    if (lambda != 0)
      (0 until vectVars.length).foreach(i => gradient.vect(i) = currentParameters.vect(i) * lambda * -2)

    score(currentParameters)
  }
}
