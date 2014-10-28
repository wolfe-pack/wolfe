package ml.wolfe.apps.factorization

import cc.factorie.la.DenseTensor1
import cc.factorie.optimize.{AdaGrad, BatchTrainer, LBFGS, OnlineTrainer}
import ml.wolfe.apps.TensorKB
import ml.wolfe.apps.factorization.io.{EvaluateNAACL, LoadNAACL, WriteNAACL}
import ml.wolfe.fg.{CellLogisticLoss, L2Regularization, VectorMsgs}
import ml.wolfe.util.Timer
import ml.wolfe.{GradientBasedOptimizer, Wolfe}

import scala.util.Random

/**
 * @author Sebastian Riedel
 */
object MatrixFactorization extends App {
  //implicit val conf = ConfigFactory.parseFile(new File("conf/epl.conf"))
  //val k = conf.getInt("epl.relation-dim")
  val k = 100
  val λ = 0.01

  val debug = false
  val print = false

  val db = if (debug) {
    val tmp = new TensorKB(k)
    //tmp.sampleTensor(4111, 41913, 0, 0.0006893674177440786) //samples a matrix
    //tmp.sampleTensor(100, 100, 0, 0.1) //samples a matrix
    tmp
  } else LoadNAACL(k)

  val rand = new Random(0l)

  val fg = db.toFactorGraph
  val data = rand.shuffle(db.trainCells)
  val V = db.ix1ToNodeMap //cols
  val A = db.ix2ToNodeMap //rows

  def nextInit() = (rand.nextDouble() - 0.5) * 0.1
  (V.values.view ++ A.values.view).foreach(n =>
    n.variable.asVector.b = new DenseTensor1((0 until k).map(i => nextInit()).toArray))

  //println(V.values.head.variable.asVector.b)

  //most of this will potentially go into TensorKB
  for (d <- data) {
    val (colIx, rowIx, _) = d.key
    val a = A(rowIx)
    val v = V(colIx)

    //create positive fact factor
    fg.buildFactor(Seq(a, v))(_ map (_ => new VectorMsgs)) { 
      e => new CellLogisticLoss(e(0), e(1), 1.0, λ) with L2Regularization
    }

    //also create a sampled stochastic negative factor in the same column
    fg.buildStochasticFactor(Seq(v, db.sampleNode(colIx)))(_ map (_ => new VectorMsgs)) {
      e => new CellLogisticLoss(e(0), e(1), 0.0, λ) with L2Regularization
    }
  }

  fg.build()

  println(
    s"""
       |λ: $λ
       |k: $k
       |
     """.stripMargin)
  println(db.toInfoString)

  println("Optimizing...")
  Timer.time("optimization") {
    //GradientBasedOptimizer(fg, new BatchTrainer(_, new AdaGrad(), 100))
    GradientBasedOptimizer(fg, new OnlineTrainer(_, new AdaGrad(rate = 1.0), 100, 200000))
    //GradientBasedOptimizer(fg, new OnlineTrainer(_, new AdaGrad(rate = 0.1), 100, 100000))
  }
  println("Done after " + Timer.reportedVerbose("optimization"))

  if (debug && print) {
    println("train:")
    println(db.toVerboseString(showTrain = true))
    println()

    println("predicted:")
    println(db.toVerboseString())
  } else {
    WriteNAACL(db)
    EvaluateNAACL.main(Array())
  }
}


object WolfeStyleMF extends App {

  import ml.wolfe.Wolfe._
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