package ml.wolfe.apps

import breeze.optimize.StochasticGradientDescent
import cc.factorie.model.{WeightsMap, WeightsSet}
import cc.factorie.optimize.{AdaGrad, OnlineTrainer}
import ml.wolfe.FactorGraph.Node
import ml.wolfe.{GradientBasedOptimizer, FactorGraph}
import ml.wolfe.fg.{L2Regularization, VectorMsgs, CellLogisticLoss}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

/**
 * @author Sebastian Riedel
 */
object MatrixFactorization extends App {
  val k = 3
  val db = new TensorKB(k)
  db.sampleTensor(10, 10, 0, 0.1) //samples a matrix

  val fg = db.toFactorGraph
  val data = db.cells
  val V = db.ix1ToNodeMap //cols
  val A = db.ix2ToNodeMap //rows

  //most of this will potentially go into TensorKB
  for (d <- data) {
    val (colIx, rowIx, _) = d.key
    val a = A(rowIx)
    val v = V(colIx)

    //create positive fact factor
    fg.buildFactor(Seq(a, v))(_ map (_ => new VectorMsgs)) { 
      e => new CellLogisticLoss(e(0), e(1), 1.0, 0.01) with L2Regularization
    }

    //also create a sampled stochastic negative factor in the same column
    fg.buildStochasticFactor(Seq(v, db.sampleNode(colIx)))(_ map (_ => new VectorMsgs)) {
      e => new CellLogisticLoss(e(0), e(1), 0.0, 0.01) with L2Regularization
    }
  }

  fg.build()
  GradientBasedOptimizer(fg, new OnlineTrainer(_, new AdaGrad(), 100,10))

  println("train:")
  println(db.toVerboseString(showTrain = true))
  println()

  println("predicted:")
  println(db.toVerboseString())  
}