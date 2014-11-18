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
object PotentialDebugger {
  def checkGradients(potential: Potential, ε: Double = 0.00001, debug: Boolean = false): Unit = {
    val f = potential.factor
    potential.valueAndGradientForAllEdges()
    val nodes = f.edges.map(edge => (edge.n, edge.msgs.asVector.f2n))

    potential.valueForCurrentSetting()

    nodes.foreach { case (node, δ) =>

      //println(node.variable.asVector.b)

      if (debug) println(node + "\t" + node.variable.label)

      val v = node.variable.asVector.b
      (0 until v.length).foreach(i => {
        val vPos = new DenseVector(v)
        val vNeg = new DenseVector(v)

        vPos.update(i, v(i) + ε)
        vNeg.update(i, v(i) - ε)

        node.variable.asVector.setting = vPos
        val scorePos = potential.valueForCurrentSetting()

        node.variable.asVector.setting = vNeg
        val scoreNeg = potential.valueForCurrentSetting()

        val δi = (scorePos - scoreNeg) / (2 * ε)

        assert(δ.length == vPos.length)
        assert(δ.length == vNeg.length)

        if (debug) {
          val error = if (δ(i) == 0.0 && δi == 0.0) 1.0 else δi / δ(i)
          println("calc: %12.8f\tactual: %12.8f\terr: %12.8f".format(δ(i),δi,error))
        }

        if (!debug) assert(δ(i) === δi +- ε, s"Calculated gradient ${δ(1)} does not match numerical gradient $δi for node $node with label ${node.variable.label}!")

        node.variable.asVector.setting = v
      })
    }
  }
}
