package ml.wolfe.apps.factorization.util

import cc.factorie.la.DenseTensor1
import cc.factorie.optimize.{AdaGrad, OnlineTrainer, BatchTrainer}
import ml.wolfe.fg.{L2Regularization, CellLogisticLoss, VectorMsgs}
import ml.wolfe.util.ProgressLogging
import ml.wolfe.{GradientBasedOptimizer, FactorieVector}
import ml.wolfe.apps.factorization.{Cell, TensorDB}

import scala.util.Random

/**
 * @author rockt
 */
object ArgMaxSigmoid extends App {
  /**
   * Probably the most expensive way to find the argmax of the sigmoid of a dot product in history of optimization.
   * Anyway, finds the the vector that maximizes the sigmoid of the dot product of a given vector and target value.
   * @param vec a given vector
   * @param target target value
   * @return argmax_vec* σ(vec • vec*) = target
   */
  def apply(vec: FactorieVector, target: Double = 1.0, lambda: Double = 0.01): FactorieVector = {
    val db = new TensorDB(vec.length)

    db += Cell("vec1", "vec2")

    val fg = db.toFactorGraph

    val vec1Node = db.node1("vec1").get
    val vec2Node = db.node2("vec2").get

    fg.buildFactor(Seq(vec2Node, vec1Node))(_ map (_ => new VectorMsgs)) {
      e => new CellLogisticLoss(e(0), e(1), target, lambda, 1.0, false) with L2Regularization
    }

    fg.build()

    vec1Node.variable.asVector.b = vec

    GradientBasedOptimizer(fg, new OnlineTrainer(_, new AdaGrad(rate = 1.0), 1000, 1) with ProgressLogging)

    vec2Node.variable.asVector.b
  }

  val rand = new Random(0l)

  val col = new DenseTensor1((0 until 100).map(i => rand.nextGaussian() * 0.1).toArray)

  println(col)

  val row = ArgMaxSigmoid(col)


  def sig(x: Double) = 1.0 / (1.0 + math.exp(-x))

  println("vec1:           " + col.mkString("\t"))
  println("vec2:           " + row.mkString("\t"))
  println("σ(vec1 • vec2): " + sig(col dot row))
}
