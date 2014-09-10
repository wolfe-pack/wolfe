package ml.wolfe.fg

import ml.wolfe.FactorGraph._

/**
 * @author Sebastian Riedel
 */
final class CellLogisticLoss(rowEdge: Edge, columnEdge: Edge, truth:Boolean = true) extends Potential {

  //todo: incorporate truth

  //nodes of edges may change hence the def and not val.
  def rowVar = rowEdge.n.variable.asVector
  def columnVar = columnEdge.n.variable.asVector
  val rowMsgs    = rowEdge.msgs.asVector
  val columnMsgs = columnEdge.msgs.asVector

  def sig(x: Double) = 1.0 / (1.0 + math.exp(-x))

  override def valueForCurrentSetting() = {
    val a = rowVar.setting
    val v = columnVar.setting
    val s = a dot v
    val p = sig(s)
    math.log(p)
  }

  override def valueAndGradientForAllEdges() = {
    val s = rowMsgs.n2f dot columnMsgs.n2f
    val p = sig(s)
    rowMsgs.f2n = columnMsgs.n2f * (1.0 - p)
    columnMsgs.f2n = rowMsgs.n2f * (1.0 - p)
    math.log(p)
  }
}
