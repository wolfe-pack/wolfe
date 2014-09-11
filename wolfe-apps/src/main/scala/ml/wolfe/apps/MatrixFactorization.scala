package ml.wolfe.apps

import cc.factorie.optimize.{L2Regularization, AdaGrad, OnlineTrainer}
import ml.wolfe.{GradientBasedOptimizer, FactorGraph}
import ml.wolfe.fg.{VectorMsgs, CellLogisticLoss}

import scala.util.Random

/**
 * @author Sebastian Riedel
 */
object MatrixFactorization extends App {

  val k      = 100
  val random = new Random(0)

  val fg = new FactorGraph

  val numRows = 100
  val numCols = 100
  val cellDensity = 0.1
  val numObservedCells = (numRows * numCols * cellDensity).toInt

  val rows = (0 until numRows).map(i => 'e + i.toString).toArray
  val cols = (0 until numCols).map(i => 'r + i.toString).toArray

  val data = (0 until numObservedCells).map(i => {
    val row = random.nextInt(numRows)
    val col = random.nextInt(numCols)
    rows(row) -> cols(col)
  })

  val A = (rows map (p => p -> fg.addVectorNode(k))).toMap
  val V = (cols map (r => r -> fg.addVectorNode(k))).toMap

  //create positive fact factors
  for (d <- data) {
    val a = A(d._1)
    val v = V(d._2)
    fg.buildFactor(Seq(a, v))(
      _ map (_ => new VectorMsgs)) { e => new CellLogisticLoss(e(0), e(1), true) }
  }

  //create one negative stochastic factor per relation
  for (r <- cols) {
    val v = V(r)
    //todo: this should check if pair is not observed for relation.
    def sampleRow = rows(random.nextInt(numRows))
    fg.buildStochasticFactor(Seq(v, A(sampleRow))) (
      _ map (_ => new VectorMsgs)) { e => new CellLogisticLoss(e(0), e(1), false) }
  }

  fg.build()

  GradientBasedOptimizer(fg, new OnlineTrainer(_, new AdaGrad(), 100,1))

  for (p <- rows) {
    println(s"$p: ${A(p).variable.asVector.b}")
  }

  for (r <- cols) {
    println(s"$r: ${V(r).variable.asVector.b}")
  }


}
