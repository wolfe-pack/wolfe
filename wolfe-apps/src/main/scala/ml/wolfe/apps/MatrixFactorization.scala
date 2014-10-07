package ml.wolfe.apps

import breeze.optimize.StochasticGradientDescent
import cc.factorie.model.{WeightsMap, WeightsSet}
import cc.factorie.optimize.{AdaGrad, OnlineTrainer}
import ml.wolfe.FactorGraph.Node
import ml.wolfe.{Wolfe, DenseVector, GradientBasedOptimizer, FactorGraph}
import ml.wolfe.fg.{L2Regularization, VectorMsgs, CellLogisticLoss}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

/**
 * @author Sebastian Riedel
 */
object MatrixFactorization extends App {
  val k = 3
  val db = new TensorKB(k)
  db.sampleTensor(10, 10, 0, 0.1) //samples a matrix

  val fg = db.toFactorGraph
  val data = db.cells
  val V = db.ix1ToNodeMap //cols
  val A = db.ix2ToNodeMap //rows

  //most of this will potentially go into TensorKB
  for (d <- data) {
    val (colIx, rowIx, _) = d.key
    val a = A(rowIx)
    val v = V(colIx)

    //create positive fact factor
    fg.buildFactor(Seq(a, v))(_ map (_ => new VectorMsgs)) { 
      e => new CellLogisticLoss(e(0), e(1), 1.0, 0.01) with L2Regularization
    }

    //also create a sampled stochastic negative factor in the same column
    fg.buildStochasticFactor(Seq(v, db.sampleNode(colIx)))(_ map (_ => new VectorMsgs)) {
      e => new CellLogisticLoss(e(0), e(1), 0.0, 0.01) with L2Regularization
    }
  }

  fg.build()
  GradientBasedOptimizer(fg, new OnlineTrainer(_, new AdaGrad(), 100,10))

  println("train:")
  println(db.toVerboseString(showTrain = true))
  println()

  println("predicted:")
  println(db.toVerboseString())  
}


object WolfeStyleMF extends App {

  import Wolfe._
  import ml.wolfe.macros.OptimizedOperators._
  case class Data(rel:String, arg1:String, arg2:String, target:Double)

  case class Model(relationVectors:Map[String,Seq[Double]], entityPairVectors:Map[(String,String),Seq[Double]])

  def dot(a1:Seq[Double],a2:Seq[Double]) = ???

  val rels = Seq("profAt")
  val ents = Seq("Luke" -> "MIT")


  def searchSpace(k:Int) = all(Model)(maps(rels,fvectors(k)) x maps(ents,fvectors(k)))

  def fvectors(k:Int) = Wolfe.seqsOfLength(k,Wolfe.doubles)



  //@Potential(???) //cell logistic potential
  def logisticLoss(target:Double, arg1:Seq[Double], arg2:Seq[Double]) =
  //todo: sigmoid
    sum(0 until arg1.length) { i => arg1(i) * arg2(i) }

  //@Stochastic(String => (String, String)) //samples a non-observed pair efficiently from data; not for now
  //creates as many stochastic factors as the integer before the sum
  @Stochastic
  def negativeDataLoss(data: Seq[Data])(model: Model) = {
    val r = data.head.rel
    val numObserved = data.size //function of r
    val numUnobserved = ents.size - numObserved

    //there needs to be a default implementation that takes the filtered domain (ents) and samples from it
    numObserved * sum(ents filter { pair => !data.exists(d => pair == (d.arg1, d.arg2)) }){ pair =>
      logisticLoss(0.0, model.entityPairVectors(pair), model.relationVectors(r)) * (numUnobserved / numObserved.toDouble)
    }
  }

  def objective(data:Seq[Data])(model:Model) = {
    sum(data) { d => logisticLoss(d.target,model.entityPairVectors(d.arg1 -> d.arg2), model.relationVectors(d.rel)) } +
    sum(rels) { r => negativeDataLoss(data.filter(_.rel == r))(model) }
  }

  println("It compiles, yay! :)")
}