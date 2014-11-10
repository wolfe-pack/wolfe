package ml.wolfe.apps.factorization

import ml.wolfe.FactorGraph.Edge
import ml.wolfe._
import ml.wolfe.fg.{Regularization, Potential}

/**
 * A potential for a formula containing two predicates
 * @param constEdge edge to variable that refers to a constant
 * @param predicate1Edge edge to first predicate
 * @param predicate2Edge edge to second predicate
 * @param target target
 * @param lambda regularization parameter
 * @author rockt
 */
abstract class Formula2Potential(constEdge: Edge, predicate1Edge: Edge, predicate2Edge: Edge, target: Double = 1.0,
                                 val lambda: Double = 0.0, weight: Double = 1.0) extends Potential with Regularization {
  def cVar   = constEdge.n.variable.asVector
  def p1Var  = predicate1Edge.n.variable.asVector
  def p2Var  = predicate2Edge.n.variable.asVector
  val cMsgs  = constEdge.msgs.asVector
  val p1Msgs = predicate1Edge.msgs.asVector
  val p2Msgs = predicate2Edge.msgs.asVector

  def sig(x: Double) = 1.0 / (1.0 + math.exp(-x))

  private def innerLossAndDirection(s: Double): (Double, Int) =
    if (target >= s) (1 + s - target, 1)
    else (1 + target - s, -1)

  override def valueForCurrentSetting(): Double = {
    val c = cVar.setting
    val p1 = p1Var.setting
    val p2 = p2Var.setting
    val p1c = sig(c dot p1)
    val p2c = sig(c dot p2)

    val s = F(p1c, p2c)

    val loss = innerLossAndDirection(s)._1
    math.log(loss) * weight + regLoss(c) + regLoss(p1) + regLoss(p2)
  }

  override def valueAndGradientForAllEdges(): Double = {
    val p1c = sig(cMsgs.n2f dot p1Msgs.n2f)
    val p2c = sig(cMsgs.n2f dot p2Msgs.n2f)

    val s = F(p1c, p2c)

    val (loss, dir) = innerLossAndDirection(s)

    val p1c_p1 = cMsgs.n2f * p1c * (1 - p1c)
    val p1c_c = p1Msgs.n2f * p1c * (1 - p1c)
    val p2c_p2 = cMsgs.n2f * p2c * (1 - p2c)
    val p2c_c = p2Msgs.n2f * p2c * (1 - p2c)

    p1Msgs.f2n = (calcF_p1(p1c_p1, p2c) * (1.0 / loss) * dir) * weight + regGradient(p1Msgs.n2f)
    p2Msgs.f2n = (calcF_p2(p2c_p2, p1c) * (1.0 / loss) * dir) * weight + regGradient(p2Msgs.n2f)
    cMsgs.f2n = (calcF_c(p2c_c, p1c, p1c_c, p2c) * (1.0 / loss) * dir) * weight + regGradient(cMsgs.n2f)

    math.log(loss) * weight + regLoss(cMsgs.n2f) + regLoss(p1Msgs.n2f) + regLoss(p2Msgs.n2f)
  }

  /**
   * Calculates the score of a formula F that contains two predicates p1, p2.
   * @param p1c score of [p1(c)]
   * @param p2c score of [p2(c)]
   * @return score of [F]
   */
  def F(p1c: Double, p2c: Double): Double
  /**
   * Calculates gradient of [p1] in formula F.
   * @param p1c_p1 gradient of [p1] in [p1(c)]
   * @param p2c score of [p2(c)]
   * @return gradient of [p1]       
   */
  def calcF_p1(p1c_p1: FactorieVector, p2c: Double): FactorieVector
  /**
   * Calculates gradient of [p2] in formula F.
   * @param p2c_p2 gradient of [p2] in [p2(c)]
   * @param p1c score of [p1(c)]
   * @return gradient of [p2]
   */
  def calcF_p2(p2c_p2: FactorieVector, p1c: Double): FactorieVector
  /**
   * Calculates gradient of [c] in formula F.
   * @param p2c_c gradient of [c] in [p2(c)]
   * @param p1c score of [p1(c)]
   * @param p1c_c gradient of [c] in [p1(c)]
   * @param p2c score of [p2(c)]
   * @return gradient of [c]
   */
  def calcF_c(p2c_c: FactorieVector, p1c: Double, p1c_c: FactorieVector, p2c: Double): FactorieVector
}


class ImplPotential(constEdge: Edge, pred1Edge: Edge, pred2Edge: Edge, target: Double = 1.0, override val lambda: Double = 0.0, weight: Double = 1.0)
extends Formula2Potential(constEdge, pred1Edge, pred2Edge, target, lambda, weight) {
  //[p₁(c) => p₂(c)] := [p₁(c)]*([p₂(c)] - 1) + 1
  def F(p1c: Double, p2c: Double) = p1c * (p2c - 1) + 1
  def calcF_p1(p1c_p1: FactorieVector, p2c: Double) = p1c_p1 * (p2c - 1)
  def calcF_p2(p2c_p2: FactorieVector, p1c: Double) = p2c_p2 * p1c
  def calcF_c(p2c_c: FactorieVector, p1c: Double, p1c_c: FactorieVector, p2c: Double) =
    p2c_c * p1c + p1c_c * (p2c - 1)
}


class ImplNegPotential(constEdge: Edge, pred1Edge: Edge, pred2Edge: Edge, target: Double = 1.0, override val lambda: Double = 0.0, weight: Double = 1.0)
extends Formula2Potential(constEdge, pred1Edge, pred2Edge, target, lambda, weight) {
  //[p₁(c) => ¬p₂(c)] := [p₁(c)]*(-[p₂(c)]) + 1
  def F(p1c: Double, p2c: Double) = p1c * -p2c + 1
  def calcF_p1(p1c_p1: FactorieVector, p2c: Double) = p1c_p1 * -p2c
  def calcF_p2(p2c_p2: FactorieVector, p1c: Double) = p2c_p2 * -p1c
  def calcF_c(p2c_c: FactorieVector, p1c: Double, p1c_c: FactorieVector, p2c: Double) =
    p2c_c * -p1c + p1c_c * -p2c
}




class BPRPotential(const1Edge: Edge, const2Edge: Edge, predEdge: Edge, target: Double = 1.0, val lambda: Double = 0.0) extends Potential with Regularization {
  def c1Var  = const1Edge.n.variable.asVector
  def c2Var  = const2Edge.n.variable.asVector
  def pVar   = predEdge.n.variable.asVector
  val c1Msgs = const1Edge.msgs.asVector
  val c2Msgs = const2Edge.msgs.asVector
  val pMsgs  = predEdge.msgs.asVector

  def sig(x: Double) = 1.0 / (1.0 + math.exp(-x))

  private def innerLossAndDirection(s: Double): (Double, Int) =
    if (target >= s) (1 + s - target, 1)
    else (1 + target - s, -1)

  override def valueForCurrentSetting(): Double = ???

  override def valueAndGradientForAllEdges(): Double = ???
}