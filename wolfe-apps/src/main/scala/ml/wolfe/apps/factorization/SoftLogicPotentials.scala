package ml.wolfe.apps.factorization

import ml.wolfe.FactorGraph.Edge
import ml.wolfe._
import ml.wolfe.fg.{Regularization, Potential}

/**
 * Created by rockt on 29/10/2014.
 */
abstract class TwoPredicatesPotential(constEdge: Edge, predicate1Edge: Edge, predicate2Edge: Edge, target: Double = 1.0, val λ: Double = 0.0) extends Potential with Regularization {
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

    val prob = innerLossAndDirection(s)._1
    math.log(prob) + regLoss(c) + regLoss(p1) + regLoss(p2)
  }

  override def valueAndGradientForAllEdges(): Double = {
    val p1c = sig(cMsgs.n2f dot p1Msgs.n2f)
    val p2c = sig(cMsgs.n2f dot p2Msgs.n2f)

    val s = F(p1c, p2c)

    val (prob, dir) = innerLossAndDirection(s)

    val δp1c_p1 = cMsgs.n2f * p1c * (1 - p1c)
    val δp1c_c = p1Msgs.n2f * p1c * (1 - p1c)
    val δp2c_p2 = cMsgs.n2f * p2c * (1 - p2c)
    val δp2c_c = p2Msgs.n2f * p2c * (1 - p2c)

    p1Msgs.f2n = (δF_p1(δp1c_p1, p2c) * (1.0 - prob) * dir) + regGradient(p1Msgs.n2f)
    p2Msgs.f2n = (δF_p2(δp2c_p2, p1c) * (1.0 - prob) * dir) + regGradient(p2Msgs.n2f)
    cMsgs.f2n = (δF_c(δp1c_c, p1c, δp2c_c, p2c) * (1.0 - prob) * dir) + regGradient(cMsgs.n2f)

    math.log(prob) + regLoss(cMsgs.n2f) + regLoss(p1Msgs.n2f) + regLoss(p2Msgs.n2f)
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
   * @param δp1c_p1 gradient of [p1] in [p1(c)]
   * @param p2c score of [p2(c)]
   * @return gradient of [p1]
   */
  def δF_p1(δp1c_p1: FactorieVector, p2c: Double): FactorieVector
  /**
   * Calculates gradient of [p2] in formula F.
   * @param δp2c_p2 gradient of [p2] in [p2(c)]
   * @param p1c score of [p1(c)]
   * @return gradient of [p2]
   */
  def δF_p2(δp2c_p2: FactorieVector, p1c: Double): FactorieVector
  /**
   * Calculates gradient of [c] in formula F.
   * @param δp2c_c gradient of [c] in [p2(c)]
   * @param p1c score of [p1(c)]
   * @param δp1c_c gradient of [c] in [p1(c)]
   * @param p2c score of [p2(c)]
   * @return gradient of [c]
   */
  def δF_c(δp2c_c: FactorieVector, p1c: Double, δp1c_c: FactorieVector, p2c: Double): FactorieVector
}


class ImplPotential(constEdge: Edge, pred1Edge: Edge, pred2Edge: Edge, target: Double = 1.0, override val λ: Double = 0.0)
extends TwoPredicatesPotential(constEdge, pred1Edge, pred2Edge, target, λ) {
  //[p₁(c) => p₂(c)] := [p₁(c)]*([p₂(c)] - 1) + 1
  def F(p1c: Double, p2c: Double) = p1c * (p2c - 1) + 1
  def δF_p1(δp1c_p1: FactorieVector, p2c: Double) = δp1c_p1 * (p2c - 1)
  def δF_p2(δp2c_p2: FactorieVector, p1c: Double) = δp2c_p2 * p1c
  def δF_c(δp2c_c: FactorieVector, p1c: Double, δp1c_c: FactorieVector, p2c: Double) =
    δp2c_c * p1c + δp1c_c * (p2c - 1)
}


class ImplNegPotential(constEdge: Edge, pred1Edge: Edge, pred2Edge: Edge, target: Double = 1.0, override val λ: Double = 0.0)
extends TwoPredicatesPotential(constEdge, pred1Edge, pred2Edge, target, λ) {
  //[p₁(c) => ¬p₂(c)] := [p₁(c)]*(-[p₂(c)]) + 1
  def F(p1c: Double, p2c: Double) = p1c * -p2c + 1
  def δF_p1(δp1c_p1: FactorieVector, p2c: Double) = δp1c_p1 * -p2c
  def δF_p2(δp2c_p2: FactorieVector, p1c: Double) = δp2c_p2 * -p1c
  def δF_c(δp2c_c: FactorieVector, p1c: Double, δp1c_c: FactorieVector, p2c: Double) =
    δp2c_c * -p1c + δp1c_c * -p2c
}

