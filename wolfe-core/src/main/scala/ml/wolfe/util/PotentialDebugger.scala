package ml.wolfe.util

import cc.factorie.la.DenseTensor1
import ml.wolfe.{FactorGraph, DenseVector, FactorieVector}
import ml.wolfe.fg.{L2Regularization, CellLogisticLoss, VectorMsgs, Potential}
import org.scalautils.Tolerance._
import org.scalautils.TripleEquals._

import scala.util.Random

/**
 * Created by rockt on 29/10/2014.
 */
object PotentialDebugger extends App {
  def checkGradients(potential: Potential, ε: Double = 0.00001): Unit = {
    val f = potential.factor
    potential.valueAndGradientForAllEdges()
    val nodes = f.edges.map(edge => (edge.n, edge.msgs.asVector.f2n))

    potential.valueForCurrentSetting()

    nodes.foreach { case (node, δ) =>
      val v = node.variable.asVector.b
      (0 until v.length).foreach(i => {
        val vPos = new DenseVector(v)
        val vNeg = new DenseVector(v)

        vPos.update(i, v(i) + ε)
        vNeg.update(i, v(i) - ε)

        //node.edges.foreach(e => e.msgs.asVector.n2f = vPos)
        node.variable.asVector.setting = vPos
        val scorePos = potential.valueForCurrentSetting()

        node.variable.asVector.setting = vNeg
        //node.edges.foreach(e => e.msgs.asVector.n2f = vNeg)
        val scoreNeg = potential.valueForCurrentSetting()

        val δi = (scorePos - scoreNeg) / (2 * ε)

        assert(δ(i) === δi +- ε, s"Calculated gradient ${δ(1)} does not match numerical gradient $δi")
        node.variable.asVector.setting = v
      })
    }
  }

  val fg = new FactorGraph()
  val n1 = fg.addVectorNode(100)
  val n2 = fg.addVectorNode(100)

  fg.buildFactor(Seq(n1, n2))(_ map (_ => new VectorMsgs)) {
    e => new CellLogisticLoss(e(0), e(1), 1.0, 0.01) with L2Regularization
  }
  fg.build()
  val rand = new Random(0l)
  def nextInit() = rand.nextGaussian() * 0.1
  Seq(n1, n2).foreach(n => {
    val vec = new DenseVector((0 until 100).map(i => nextInit()).toArray)
    n.variable.asVector.b = vec
    n.variable.asVector.setting = vec
  })
  fg.factors.foreach(_.edges.foreach(e => e.msgs.asVector.n2f = e.n.variable.asVector.b))

  fg.edges.map(_.f.potential).foreach(checkGradients(_))
}
