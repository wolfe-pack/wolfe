package ml.wolfe.fg

import cc.factorie.la.SparseTensor1
import ml.wolfe.FactorGraph._
import ml.wolfe.FactorieVector

/**
 * @author Sebastian Riedel
 */
class CellLogisticLoss(rowEdge: Edge, columnEdge: Edge, truth: Double = 1.0, val λ: Double = 0.0) extends Potential with Regularization {
  //nodes of edges may change hence the def and not val.
  def rowVar = rowEdge.n.variable.asVector
  def columnVar = columnEdge.n.variable.asVector
  val rowMsgs    = rowEdge.msgs.asVector
  val columnMsgs = columnEdge.msgs.asVector

  def sig(x: Double) = 1.0 / (1.0 + math.exp(-x))

  private def innerLossAndDirection(s: Double): (Double, Int) =
    if (truth >= s) (1 + s - truth, 1)
    else (1 + truth - s, -1)

  override def valueForCurrentSetting(): Double = {
    val a = rowVar.setting
    val v = columnVar.setting
    val s = sig(a dot v)
    val p = innerLossAndDirection(s)._1
    math.log(p) + regLoss(a) + regLoss(v)
  }

  override def valueAndGradientForAllEdges(): Double = {
    val s = sig (rowMsgs.n2f dot columnMsgs.n2f)
    val (p, dir) = innerLossAndDirection(s)
    rowMsgs.f2n = (columnMsgs.n2f * (1.0 - p) * dir) + regGradient(rowMsgs.n2f)
    columnMsgs.f2n = (rowMsgs.n2f * (1.0 - p) * dir) + regGradient(columnMsgs.n2f)
    math.log(p) + regLoss(rowMsgs.n2f) + regLoss(columnMsgs.n2f)
  }
}

trait Regularization {
  def regLoss(vector: FactorieVector): Double = 0
  //all zeros
  def regGradient(vector: FactorieVector): FactorieVector = new SparseTensor1(vector.length)
}

/**
 * 1/2 * λ||x||²
 */
trait L2Regularization extends Regularization {
  val λ: Double
  override def regLoss(vector: FactorieVector): Double =
    if (λ == 0) 0
    else vector dot vector * λ * 0.5
  override def regGradient(vector: FactorieVector): FactorieVector =
    if (λ == 0) new SparseTensor1(vector.length)
    else vector * λ
}