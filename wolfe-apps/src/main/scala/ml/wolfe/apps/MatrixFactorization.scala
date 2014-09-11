package ml.wolfe.apps

import cc.factorie.optimize.{L2Regularization, AdaGrad, OnlineTrainer}
import ml.wolfe.{GradientBasedOptimizer, FactorGraph}
import ml.wolfe.fg.{VectorMsgs, CellLogisticLoss}

import scala.util.Random

/**
 * @author Sebastian Riedel
 */
object MatrixFactorization extends App {

  val k      = 2
  val random = new Random(0)

  val fg = new FactorGraph

  val entityPairs = Seq('A -> 'B, 'A -> 'C, 'B -> 'C)
  val relations   = Seq('rel1, 'rel2, 'rel3)

  val data = Seq(entityPairs(0) -> relations(1), entityPairs(0) -> relations(2))

  val A = (entityPairs map (p => p -> fg.addVectorNode(k))).toMap
  val V = (relations map (r => r -> fg.addVectorNode(k))).toMap

  //create positive fact factors
  for (d <- data) {
    val a = A(d._1)
    val v = V(d._2)
    fg.addFactorAndPot(Seq(a, v))(
      _ map (_ => new VectorMsgs)) { e => new CellLogisticLoss(e(0), e(1), true) }
  }

  //create one negative stochastic factor per relation
  for (r <- relations) {
    val v = V(r)
    //todo: this should check if pair is not observed for relation.
    def samplePair = entityPairs(random.nextInt(entityPairs.size))
    fg.addStochasticFactorAndPot(Seq(v, A(samplePair))) (
      _ map (_ => new VectorMsgs)) { e => new CellLogisticLoss(e(0), e(1), false) }
  }

  fg.build()

  GradientBasedOptimizer(fg, new OnlineTrainer(_, new AdaGrad(), 100,1))

  for (p <- entityPairs) {
    println(s"$p: ${A(p).variable.asVector.b}")
  }

  for (r <- relations) {
    println(s"$r: ${V(r).variable.asVector.b}")
  }


}
