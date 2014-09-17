package ml.wolfe.fg

import ml.wolfe.FactorGraph._

/**
 * @author Sebastian Riedel
 */
final class CellLogisticLoss(rowEdge: Edge, columnEdge: Edge, truth: Double = 1.0) extends Potential {

  //todo: incorporate truth
  //rockt: truth should be degree of truth in the interval  (0, 1)

  //nodes of edges may change hence the def and not val.
  def rowVar = rowEdge.n.variable.asVector
  def columnVar = columnEdge.n.variable.asVector
  val rowMsgs    = rowEdge.msgs.asVector
  val columnMsgs = columnEdge.msgs.asVector

  def sig(x: Double) = 1.0 / (1.0 + math.exp(-x))

  private def innerLossAndDirection(s: Double): (Double, Int) =
    if (truth >= s) (1 + s - truth, 1)
    else (1 + truth - s, -1)

  override def valueForCurrentSetting() = {
    val a = rowVar.setting
    val v = columnVar.setting
    val s = sig(a dot v)
    val p = innerLossAndDirection(s)._1
    math.log(p)
  }

  override def valueAndGradientForAllEdges() = {
    val s = sig (rowMsgs.n2f dot columnMsgs.n2f)
    val (p, dir) = innerLossAndDirection(s)
    rowMsgs.f2n = columnMsgs.n2f * (1.0 - p) * dir
    columnMsgs.f2n = rowMsgs.n2f * (1.0 - p) * dir
    math.log(p)
  }
}
