package ml.wolfe.apps

import breeze.optimize.StochasticGradientDescent
import cc.factorie.model.{WeightsMap, WeightsSet}
import cc.factorie.optimize.{AdaGrad, OnlineTrainer}
import ml.wolfe.{GradientBasedOptimizer, FactorGraph}
import ml.wolfe.fg.{L2Regularization, VectorMsgs, CellLogisticLoss}

import scala.util.Random

/**
 * @author Sebastian Riedel
 */
object MatrixFactorization extends App {

  val k      = 10
  val random = new Random(0)

  val fg = new FactorGraph

  val numRows = 10
  val numCols = 10
  val cellDensity = 0.5
  val numObservedCells = (numRows * numCols * cellDensity).toInt

  val rows = (0 until numRows).map(i => 'e + i.toString).toArray
  val cols = (0 until numCols).map(i => 'r + i.toString).toArray

  val data = (0 until numObservedCells).map(i => {
    val row = random.nextInt(numRows)
    val col = random.nextInt(numCols)
    rows(row) -> cols(col)
  }).toSet

  val A = (rows map (p => p -> fg.addVectorNode(k))).toMap
  val V = (cols map (r => r -> fg.addVectorNode(k))).toMap

  //create positive fact factors
  for (d <- data) {
    val a = A(d._1)
    val v = V(d._2)
    fg.buildFactor(Seq(a, v))(
      _ map (_ => new VectorMsgs)) { e => new CellLogisticLoss(e(0), e(1), 1.0, 0.01) with L2Regularization }
  }

  //create one negative stochastic factor per relation
  for (r <- cols) {
    val v = V(r)
    //todo: this should check if pair is not observed for relation.
    def sampleRow = rows(random.nextInt(numRows))
    fg.buildStochasticFactor(Seq(v, A(sampleRow))) (
      _ map (_ => new VectorMsgs)) { e => new CellLogisticLoss(e(0), e(1), 0.0, 0.01) with L2Regularization }
  }

  fg.build()



  GradientBasedOptimizer(fg, new OnlineTrainer(_, new AdaGrad(), 100,1))


  def sig(x: Double) = 1.0 / (1.0 + math.exp(-x))

  println("train:")
  println("\t" + cols.mkString(" "*3))
  println(rows.map(r => r + "\t" + cols.map(c =>
    if (data.contains((r, c))) " 1  " else " "*4
  ).mkString("  ")).mkString("\n"))

  println("predicted:")
  println("\t" + cols.mkString(" "*3))
  println(rows.map(r => r + "\t" + cols.map(c =>
    "%4.2f".format(sig(A(r).variable.asVector.b dot V(c).variable.asVector.b))
  ).mkString("  ")).mkString("\n"))
}
