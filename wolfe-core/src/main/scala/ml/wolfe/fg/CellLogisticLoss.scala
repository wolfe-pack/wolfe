package ml.wolfe.fg

import cc.factorie.la._
import ml.wolfe.FactorGraph._
import ml.wolfe.FactorieVector
import ml.wolfe.DenseVector
import ml.wolfe.util.Conf

/**
 * @author Sebastian Riedel
 * @author rockt
 */
class CellLogisticLoss(rowEdge: Edge, columnEdge: Edge, target: Double = 1.0, val lambda: Double = 0.0, weight: Double = 1.0, updateCol: Boolean = true) extends Potential with Regularization {
  //nodes of edges may change hence the def and not val.
  def rowVar = rowEdge.n.variable.asVector
  def columnVar = columnEdge.n.variable.asVector
  val rowMsgs    = rowEdge.msgs.asVector
  val columnMsgs = columnEdge.msgs.asVector

  def sig(x: Double) = 1.0 / (1.0 + math.exp(-x))

  private def innerLossAndDirection(s: Double): (Double, Int) =
    if (target >= s) (1 + s - target, 1)
    else (1 + target - s, -1)

  override def valueForCurrentSetting(): Double = {
    val a = rowVar.setting
    val v = columnVar.setting
    val s = sig(a dot v)
    val loss = innerLossAndDirection(s)._1
    math.log(loss) * weight + regLoss(a) + regLoss(v)
  }

  override def valueAndGradientForAllEdges(): Double = {
    val s = sig(rowMsgs.n2f dot columnMsgs.n2f)
    val (loss, dir) = innerLossAndDirection(s)
    rowMsgs.f2n = (columnMsgs.n2f * (1.0 - loss) * dir) * weight + regGradient(rowMsgs.n2f)
    columnMsgs.f2n =
      if (updateCol) (rowMsgs.n2f * (1.0 - loss) * dir) * weight + regGradient(columnMsgs.n2f)
      else new SparseTensor1(columnMsgs.n2f.length)
    math.log(loss) * weight + regLoss(rowMsgs.n2f) + regLoss(columnMsgs.n2f)
  }
}

/**
 * @author sameer
 */
class CellLogisticLossWithRowFeatures(rowEdge: Edge, columnEdge: Edge, val colFeatureWeights: Edge, val rowFeatures: Edge, target: Double = 1.0, lambda: Double = 0.0, weight: Double = 1.0)
  extends CellLogisticLoss(rowEdge, columnEdge, target, lambda, weight) {
  //nodes of edges may change hence the def and not val.
  def columnFeatWeights = colFeatureWeights.n.variable.asVector
  val columnFtrWMsgs = colFeatureWeights.msgs.asVector
  def rowFeats = rowFeatures.n.variable.asVector
  val rowFtrMsgs = rowFeatures.msgs.asVector

  private def innerLossAndDirection(s: Double): (Double, Int) =
    if (target >= s) (1 + s - target, 1)
    else (1 + target - s, -1)

  override def valueForCurrentSetting(): Double = {
    val a = rowVar.setting
    val v = columnVar.setting
    val w = columnFeatWeights.setting
    val f = rowFeats.b
    val s = sig((a dot v) + (w dot f))
    val loss = innerLossAndDirection(s)._1
    math.log(loss) * weight + regLoss(a) + regLoss(v) + regLoss(w)
  }

  override def valueAndGradientForAllEdges(): Double = {
    val s = sig((rowMsgs.n2f dot columnMsgs.n2f) + (rowFeats.b dot columnFtrWMsgs.n2f))
    val (loss, dir) = innerLossAndDirection(s)
    rowMsgs.f2n = (columnMsgs.n2f * (1.0 - loss) * dir) * weight + regGradient(rowMsgs.n2f)
    columnMsgs.f2n = (rowMsgs.n2f * (1.0 - loss) * dir) * weight + regGradient(columnMsgs.n2f)
    columnFtrWMsgs.f2n = new SparseTensor1(rowFeats.b.dim1) //regGradient(columnFtrWMsgs.n2f)
    for(idx <- rowFeats.b.activeDomain) {
      columnFtrWMsgs.f2n(idx) = columnFtrWMsgs.f2n(idx) + (1.0 - loss) * dir * weight
    }
    rowFtrMsgs.f2n = new SparseTensor1(rowFeats.b.dim1)
    math.log(loss) * weight + regLoss(rowMsgs.n2f) + regLoss(columnMsgs.n2f)
  }
}

/**
 * @author Sebastian Riedel
 */
class CellLogisticLoss2(rowEdge: Edge, columnEdge: Edge, truth: Double = 1.0) extends Potential  {
  //nodes of edges may change hence the def and not val.
  def rowVar = rowEdge.n.variable.asVector
  def columnVar = columnEdge.n.variable.asVector
  val rowMsgs    = rowEdge.msgs.asVector
  val columnMsgs = columnEdge.msgs.asVector



  override def valueAndGradientForAllEdges(): Double = {
    val s = rowMsgs.n2f dot columnMsgs.n2f
    val Z = 1 + math.exp(s)
    val logZ = math.log(Z)
    val o = truth * s - logZ
    val p = math.exp(s - logZ)
    rowMsgs.f2n = columnMsgs.n2f * (truth - p)
    columnMsgs.f2n = rowMsgs.n2f * (truth - p)
    o
  }
}


trait Regularization {
  def regLoss(vector: FactorieVector): Double = 0
  //all zeros
  def regGradient(vector: FactorieVector): FactorieVector = new SparseTensor1(vector.length)
}

/**
 * λ||x||²
 */
trait L2Regularization extends Regularization {
  val lambda: Double
  override def regLoss(vector: FactorieVector): Double =
    if (lambda == 0) 0
    else -lambda * vector.twoNormSquared
  override def regGradient(vector: FactorieVector): FactorieVector =
    if (lambda == 0) new SparseTensor1(vector.length)
    else vector * lambda * -2
}


/**
 * @author rockt
 *
 * Bayesian Pairwise Ranking objective as in Rendle, Steffen, et al. "BPR: Bayesian personalized ranking from implicit feedback." Proceedings of the Twenty-Fifth Conference on Uncertainty in Artificial Intelligence. AUAI Press, 2009.
 *
 * @param const1Edge edge to node that represents an entity pair that has been observed in the relation
 * @param const2Edge edge to node that represents an entity pair that has *not* been observed in the relation
 * @param predEdge edge to node that represents the relation
 * @param target target value
 * @param lambda regualarization parameter
 */
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

  override def valueForCurrentSetting(): Double = {
    val c1 = c1Var.setting
    val c2 = c2Var.setting
    val p = pVar.setting
    val s = sig((p dot c1) - (p dot c2))

    val loss = innerLossAndDirection(s)._1
    math.log(loss) + regLoss(c1) + regLoss(c2) + regLoss(p)
  }

  override def valueAndGradientForAllEdges(): Double = {
    val c1 = c1Msgs.n2f
    val c2 = c2Msgs.n2f
    val p = pMsgs.n2f
    val s = sig((p dot c1) - (p dot c2))

    val (loss, dir) = innerLossAndDirection(s)

    c1Msgs.f2n = p * ((1.0 - loss) * dir) + regGradient(c1)
    c2Msgs.f2n = p * -((1.0 - loss) * dir) + regGradient(c2)
    pMsgs.f2n = (c1 - c2) * ((1.0 - loss) * dir) + regGradient(p)

    math.log(loss) + regLoss(c1) + regLoss(c2) + regLoss(p)
  }
}


/**
 * @author rockt
 */
class Tucker3CellLogisticLoss(coreEdge: Edge, rowEdge: Edge, columnEdge: Edge, layerEdge: Edge, target: Double = 1.0, val lambda: Double = 0.0, weight: Double = 1.0) extends Potential with Regularization {
  import FactorieTensor3._

  def coreVar    = coreEdge.n.variable.asVector
  def rowVar     = rowEdge.n.variable.asVector
  def columnVar  = columnEdge.n.variable.asVector
  def layerVar   = layerEdge.n.variable.asVector
  val coreMsgs   = coreEdge.msgs.asVector
  val rowMsgs    = rowEdge.msgs.asVector
  val columnMsgs = columnEdge.msgs.asVector
  val layerMsgs  = layerEdge.msgs.asVector

  def sig(x: Double) = 1.0 / (1.0 + math.exp(-x))

  private def innerLossAndDirection(s: Double): (Double, Int) =
    if (target >= s) (1 + s - target, 1)
    else (1 + target - s, -1)

  override def valueForCurrentSetting(): Double = {
    val G = coreVar.setting
    val p = columnVar.setting
    val c1 = rowVar.setting
    val c2 = layerVar.setting

    val s = sig((G mode1Product p mode2Product c1) dot c2)
    val loss = innerLossAndDirection(s)._1
    math.log(loss) * weight + regLoss(G) + regLoss(p) + regLoss(c1) + regLoss(c2)
  }

  override def valueAndGradientForAllEdges(): Double = {
    val G = coreMsgs.n2f
    val p = columnMsgs.n2f
    val c1 = rowMsgs.n2f
    val c2 = layerMsgs.n2f

    val s = sig((G mode1Product p mode2Product c1) dot c2)
    val (loss, dir) = innerLossAndDirection(s)

    coreMsgs.f2n = ((p outer c1 outer c2) * (1.0 - loss) * dir) * weight + regGradient(rowMsgs.n2f)
    columnMsgs.f2n = ((G mode2Product c1 mode3Product c2) * (1.0 - loss) * dir) * weight + regGradient(columnMsgs.n2f)
    rowMsgs.f2n = ((G mode1Product p mode3Product c2) * (1.0 - loss) * dir) * weight + regGradient(columnMsgs.n2f)
    layerMsgs.f2n = ((G mode1Product p mode2Product c1) * (1.0 - loss) * dir) * weight + regGradient(columnMsgs.n2f)

    math.log(loss) * weight + regLoss(G) + regLoss(p) + regLoss(c1) + regLoss(c2)
  }
}

object FactorieTensor3 extends App {
  type FactorieMatrix = Tensor2

  implicit class Tensor3AsVector(inner: FactorieVector) {
    def mode1Product(vector: FactorieVector): FactorieMatrix = ???
    def mode2Product(vector: FactorieVector): FactorieMatrix = ???
    def mode3Product(vector: FactorieVector): FactorieMatrix = ???

    def toTensor3String(dim1: Int, dim2: Int, dim3: Int): String =
      (0 until dim1).map(row =>
        (0 until dim2).map(col => {
          (0 until dim3).map(layer => {
            inner(row * dim1 + col * dim2 + layer * dim3)
          })
        })
      ).mkString("")
  }

  implicit class Tensor3AsMatrix(inner: FactorieMatrix) {
    //def mode1Product(vector: FactorieVector): FactorieVector = ???
    def mode2Product(vector: FactorieVector): FactorieVector = ???
    def mode3Product(vector: FactorieVector): FactorieVector = ???
  }

  implicit def tensorToVector(tensor: Tensor): FactorieVector = new DenseVector(tensor.asArray)


  val tensor3 = new DenseVector(Array(
    1.0, 0.0,
    0.0, 1.0,
  //
    0.0, 0.0,
    1.0, 1.0
  ))

  println(tensor3.toTensor3String(2, 2, 2))
}