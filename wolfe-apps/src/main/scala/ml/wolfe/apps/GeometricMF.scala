package ml.wolfe.apps

import cc.factorie.optimize.{BatchTrainer, AdaGrad, OnlineTrainer}
import ml.wolfe.FactorGraph.Edge
import ml.wolfe.GradientBasedOptimizer
import ml.wolfe.fg._

/**
 * @author Sebastian Riedel
 */
/**
 * @author Sebastian Riedel
 */
object GeometricMF extends App {
  val k  = 2
  val db = new TensorKB(k)
  db.sampleTensor(10, 10, 0, 0.1)
  //samples a matrix

  val fg   = db.toFactorGraph
  val data = db.cells
  val V    = db.ix1ToNodeMap
  //cols
  val A    = db.ix2ToNodeMap //rows

  //most of this will potentially go into TensorKB
  for (d <- data) {
    val (colIx, rowIx, _) = d.key
    val a = A(rowIx)
    val v = V(colIx)

    //create positive fact factor
    fg.buildFactor(Seq(a, v))(_ map (_ => new VectorMsgs)) {
      e => new CellLogisticLoss2(e(0), e(1), 1.0, 0.0) with L2Regularization
    }
  }

  //create repulsion factors
  for (relation1 <- 0 until db.relations.length; relation2 <- relation1 + 1 until db.relations.size) {
    val v1 = V(db.relations(relation1))
    val v2 = V(db.relations(relation2))
    fg.buildFactor(Seq(v1, v2))(_ map (_ => new VectorMsgs)) {
      e => new CellLogisticLoss2(e(0), e(1), 0.0, 0.0) with L2Regularization
    }
  }

  //create L2 regularizer for columns
  for (rel <- db.relations) {
    val v = V(rel)
    fg.buildFactor(Seq(v))(_ map (_ => new VectorMsgs)) {
      e => new L2Regularizer(e(0), 0.1)
    }
  }

  //create L2 regularizer for rows
  for (ent <- db.arg1s) {
    val a = A(ent)
    fg.buildFactor(Seq(a))(_ map (_ => new VectorMsgs)) {
      e => new L2Regularizer(e(0), 0.1)
    }
  }


  fg.build()
  //GradientBasedOptimizer(fg, new OnlineTrainer(_, new AdaGrad(), 100, 10))
  GradientBasedOptimizer(fg, new BatchTrainer(_, new AdaGrad(), 100))

  println("train:")
  println(db.toVerboseString(showTrain = true))
  println()

  println("predicted:")
  println(db.toVerboseString())
}


/**
 * @author Sebastian Riedel
 */
class L2PairwiseRegularizer(col1Edge: Edge, col2Edge: Edge, scale: Double = 1.0) extends Potential {
  //nodes of edges may change hence the def and not val.
  def col1Var = col1Edge.n.variable.asVector
  def col2Var = col2Edge.n.variable.asVector
  val col1Msgs = col1Edge.msgs.asVector
  val col2Msgs = col2Edge.msgs.asVector

  override def valueForCurrentSetting(): Double = {
    val v1 = col1Var.setting
    val v2 = col2Var.setting
    val s = v1 dot v2
    scale * s
  }

  override def valueAndGradientForAllEdges(): Double = {
    col1Msgs.f2n = col2Msgs.n2f * scale
    col2Msgs.f2n = col1Msgs.n2f * scale
    val score = scale * (col2Msgs.n2f dot col1Msgs.n2f)
    score
  }
}

/**
 * @author Sebastian Riedel
 */
class L2Regularizer(col1Edge: Edge, scale: Double = 1.0) extends Potential {
  //nodes of edges may change hence the def and not val.
  def col1Var = col1Edge.n.variable.asVector
  val col1Msgs = col1Edge.msgs.asVector

  override def valueForCurrentSetting(): Double = {
    val v1 = col1Var.setting
    val s = v1 dot v1
    -scale * s
  }

  override def valueAndGradientForAllEdges(): Double = {
    col1Msgs.f2n = col1Msgs.n2f * (-scale)
    val score = -scale * (col1Msgs.n2f dot col1Msgs.n2f)
    score
  }
}
/**
 * @author Sebastian Riedel
 */
class LogRepulsion(col1Edge: Edge, col2Edge: Edge, scale: Double = 1.0) extends Potential {
  //nodes of edges may change hence the def and not val.
  def col1Var = col1Edge.n.variable.asVector
  def col2Var = col2Edge.n.variable.asVector
  val col1Msgs = col1Edge.msgs.asVector
  val col2Msgs = col2Edge.msgs.asVector

  override def valueForCurrentSetting(): Double = {
    val v1 = col1Var.setting
    val v2 = col2Var.setting
    val s = v1 dot v2
    -scale * s
  }

  override def valueAndGradientForAllEdges(): Double = {
    col1Msgs.f2n = col2Msgs.n2f * (-scale)
    col2Msgs.f2n = col1Msgs.n2f * (-scale)
    val score = -scale * (col2Msgs.n2f dot col1Msgs.n2f)
    score
  }
}
