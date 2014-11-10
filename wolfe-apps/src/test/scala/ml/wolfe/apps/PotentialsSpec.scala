package ml.wolfe.apps

import ml.wolfe.util.PotentialDebugger
import ml.wolfe.{DenseVector, FactorGraph}
import ml.wolfe.apps.factorization.{ImplNegPotential, ImplPotential}
import ml.wolfe.fg.{BPRPotential, L2Regularization, CellLogisticLoss, VectorMsgs}

import scala.util.Random

/**
 * @author rockt
 */
object PotentialsSpec extends App {
  //building factor graph
  val fg = new FactorGraph()
  val n1 = fg.addVectorNode(100, "c")
  val n2 = fg.addVectorNode(100, "p1")
  val n3 = fg.addVectorNode(100, "p2")
  val n4 = fg.addVectorNode(100, "c2")

  val lambda = 0.01

  fg.buildFactor(Seq(n1, n2))(_ map (_ => new VectorMsgs)) {
    e => new CellLogisticLoss(e(0), e(1), 1.0, lambda) with L2Regularization
  }

  fg.buildFactor(Seq(n1, n3))(_ map (_ => new VectorMsgs)) {
    e => new CellLogisticLoss(e(0), e(1), 0.0, lambda) with L2Regularization
  }

  fg.buildFactor(Seq(n1, n3))(_ map (_ => new VectorMsgs)) {
    e => new CellLogisticLoss(e(0), e(1), 0.0, lambda, 0.5) with L2Regularization
  }

  fg.buildFactor(Seq(n1, n2, n3))(_ map (_ => new VectorMsgs)) {
    e => new ImplPotential(e(0), e(1), e(2), 1.0, lambda) with L2Regularization
  }

  fg.buildFactor(Seq(n1, n2, n3))(_ map (_ => new VectorMsgs)) {
    e => new ImplPotential(e(0), e(1), e(2), 1.0, lambda, 10.0) with L2Regularization
  }

  fg.buildFactor(Seq(n1, n3, n2))(_ map (_ => new VectorMsgs)) {
    e => new ImplNegPotential(e(0), e(1), e(2), 1.0, lambda) with L2Regularization
  }

  fg.buildFactor(Seq(n1, n3, n2))(_ map (_ => new VectorMsgs)) {
    e => new ImplNegPotential(e(0), e(1), e(2), 1.0, lambda, 0.5) with L2Regularization
  }


  fg.buildFactor(Seq(n1, n4, n2))(_ map (_ => new VectorMsgs)) {
    e => new BPRPotential(e(0), e(1), e(2), 1.0, lambda) with L2Regularization
  }

  fg.build()

  //initializing weights and messages
  val rand = new Random(0l)
  def nextInit() = rand.nextGaussian() * 0.1
  Seq(n1, n2, n3, n4).foreach(n => {
    val vec = new DenseVector((0 until 100).map(i => nextInit()).toArray)
    n.variable.asVector.b = vec
    n.variable.asVector.setting = vec
  })
  fg.factors.foreach(_.edges.foreach(e => e.msgs.asVector.n2f = e.n.variable.asVector.b))

  //gradient checking
  fg.factors.map(_.potential).foreach(PotentialDebugger.checkGradients(_, debug = true))
}
