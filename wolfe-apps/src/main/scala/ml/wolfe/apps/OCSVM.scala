package ml.wolfe.apps

/*import cc.factorie.optimize.{LBFGS, AdaGrad, OnlineTrainer}
import ml.wolfe.nlp.{CharOffsets, Token, DependencyTree}
import ml.wolfe.{BeliefPropagation, Wolfe}
import ml.wolfe.fg.TreePotential*/

import cc.factorie.la.{DenseTensor1, Tensor1}
import cc.factorie.optimize.{AdaGrad, OnlineTrainer}
import ml.wolfe._
import ml.wolfe.fg.{VectorMsgs, DiscreteMsgs, Potential}

import ml.wolfe.Wolfe._
import ml.wolfe.util.ProgressLogging

class OneClassSVMPerInstanceLoss(weightsEdge: FactorGraph.Edge, rhoEdge: FactorGraph.Edge,
                                 val xs: FactorieVector, val y: Double, val nu: Double, val idx:Int) extends Potential {

  override def valueAndGradientForAllEdges() = {
    val currentWeights = weightsEdge.msgs.asVector.n2f
    val currentRho = rhoEdge.msgs.asVector.n2f(0)

    // Set gradients for W
    weightsEdge.msgs.asVector.f2n = new DenseTensor1(currentWeights.length)
    for (j <- 0 until currentWeights.length) {
      weightsEdge.msgs.asVector.f2n(j) =
      if (currentRho >= (currentWeights dot xs) * y) xs(j) * y
      else 0
    }

    // Set gradient for rho

    rhoEdge.msgs.asVector.f2n = new DenseTensor1(1,
      if (currentRho >= (currentWeights dot xs) * y) -1 + nu
      else nu)

    println(currentRho)
    if(idx==1 && currentRho < (currentWeights dot xs) * y ) {
      println("omgzz")
    }
    // Return value
    -math.max((1 - nu) * currentRho - (currentWeights dot xs) * y, -nu * currentRho)
  }
}

class l2reg(weightsEdge: FactorGraph.Edge, nu: Double, m: Int) extends Potential {

  override def valueAndGradientForAllEdges() = {
    val currentWeights = weightsEdge.msgs.asVector.n2f

    // Set gradient for w
    weightsEdge.msgs.asVector.f2n = new DenseTensor1(currentWeights.length)
    for (j <- 0 until currentWeights.length) {
      weightsEdge.msgs.asVector.f2n(j) = -nu * m * currentWeights(j)
    }
    -(currentWeights dot currentWeights) * nu * m
  }
}

case class XY(xs: Seq[FactorieVector], y: FactorieVector)

object OCSVM extends App {
  val nu = 0.5

  def tens(vals: Double*) = new DenseTensor1(vals.toArray)

  val data        = Seq(
    XY(Seq(tens(0, 1, 0)), tens(1)),
    XY(Seq(tens(1, 0, 0), tens(0, 0, 1)), tens(1, 1)),
    XY(Seq(tens(0, 1, 0)), tens(1))
  )
  val R           = data.length
  val fg          = new FactorGraph()
  val weightnodes = for (i <- 0 until 3) yield
    fg.addVectorNode(R, "weight" + i)
  val rhonodes    = for (i <- 0 until 3) yield
    fg.addVectorNode(1, "rho" + i)
  for (j <- 0 until 3) {
    val m = data(j).xs.length
    for (i <- 0 until m) {
      fg.buildFactor(Seq(weightnodes(j), rhonodes(j)))(_ => Seq(new VectorMsgs, new VectorMsgs))(
        edges => new OneClassSVMPerInstanceLoss(edges(0), edges(1), data(j).xs(i), data(j).y(i), nu, j)
      )
    }
    fg.buildFactor(Seq(weightnodes(j)))(_ => Seq(new VectorMsgs))(edges => new l2reg(edges(0), nu, m))
  }

  D3Implicits.saveD3Graph(fg)

  fg.build()

  GradientBasedOptimizer(fg,
    new OnlineTrainer(_, new AdaGrad(rate = 1), 100001, fg.factors.size - 1)
  )

  for (w <- weightnodes) {
    println(w.variable.asVector.b)
  }
  for (rho <- rhonodes) {
    println(rho.variable.asVector.b)
  }

}

