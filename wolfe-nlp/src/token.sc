import ml.wolfe.{BeliefPropagation, FactorGraph}
import ml.wolfe.fg.Potential

class MyBoltzmannMachinePerInstanceLoss(weightsEdge:FactorGraph.Edge) extends Potential {
  val factorGraph = new FactorGraph
  factorGraph.addDiscreteNode(10)
  override def valueAndGradientForAllEdges() = {
    val currentWeights = weightsEdge.msgs.asVector.n2f
    //get current weights from incoming message
    BeliefPropagation.sumProduct(10)(factorGraph)
    weightsEdge.msgs.asVector.f2n = ??? //my gradient
    0.0
  }
}

class OneClassSVMPerInstanceLoss(weightsEdge:FactorGraph.Edge) extends Potential {

  val binaryClassifier = ???

  override def valueAndGradientForAllEdges() = {
    val currentWeights = weightsEdge.msgs.asVector.n2f
    weightsEdge.msgs.asVector.f2n = ??? //my gradient
    ???

  }
}