package ml.wolfe.apps.factorization

import java.io.File

import cc.factorie.la.DenseTensor1
import cc.factorie.optimize._
import ml.wolfe.FactorGraph.Edge
import ml.wolfe.apps.TensorKB
import ml.wolfe.apps.factorization.io.{EvaluateNAACL, LoadNAACL, WriteNAACL}
import ml.wolfe.fg.L2Regularization
import ml.wolfe.fg._
import ml.wolfe.util.{ProgressBar, Timer}
import ml.wolfe.{GradientBasedOptimizer, Wolfe}

import scala.util.Random

/**
 * @author Sebastian Riedel
 */
object MatrixFactorization extends App {
  //implicit val conf = ConfigFactory.parseFile(new File("conf/epl.conf"))

  //todo: all of this should go into a config file
  val outputPath = "data/out/"
  val fileName = "predict.txt"

  val k = 100
  val λ = 0.01
  val α = 0.1
  val initScale = 0.05
  val maxIter = 200

  val debug = false
  val print = false

  val db = if (debug) {
    val tmp = new TensorKB(k)
    //tmp.sampleTensor(100, 100, 0, 0.1) //samples a matrix
    tmp
  } else LoadNAACL(k)

  val rand = new Random(0l)

  val fg = db.toFactorGraph
  val data = rand.shuffle(db.trainCells)
  val V = db.ix1ToNodeMap //cols
  val A = db.ix2ToNodeMap //rows

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

  println(s"""Config:
    |λ:        $λ
    |k:        $k
    |α:        $α
    |initScale:$initScale
    |maxIter:  $maxIter""".stripMargin)

  println(db.toInfoString)

  println("Optimizing...")
  Timer.time("optimization") {
    //GradientBasedOptimizer(fg, new BatchTrainer(_, new AdaGrad(), maxIter, 200000), 0.01) //2nd best
    //GradientBasedOptimizer(fg, new BatchTrainer(_, new AdaGrad(rate = α), maxIter), initScale)
    //GradientBasedOptimizer(fg, new BatchTrainer(_, new ConstantLearningRate(baseRate = α), maxIter), initScale)
    GradientBasedOptimizer(fg, new OnlineTrainer(_, new ConstantLearningRate(baseRate = α), maxIter, 200000)) //best
    //GradientBasedOptimizer(fg, new OnlineTrainer(_, new AdaGrad(rate = α), maxIter, 200000), initScale)
    //GradientBasedOptimizer(fg, new OnlineTrainer(_, new AdaMira(rate = α), maxIter, 200000), initScale)
  }
  println("Done after " + Timer.reportedVerbose("optimization"))

  if (debug && print) {
    println("train:")
    println(db.toVerboseString(showTrain = true))
    println()

    println("predicted:")
    println(db.toVerboseString())

  } else {
    WriteNAACL(db, outputPath + fileName)
    EvaluateNAACL.main(Array("./conf/eval.conf", outputPath + fileName))

    import scala.sys.process._
    Process("pdflatex -interaction nonstopmode -shell-escape table.tex", new File(outputPath)).!!
  }
}

class SoftImplicationLoss(entityEdge: Edge, relation1Edge: Edge, relation2Edge: Edge, target: Double = 1.0, val λ: Double = 0.0) extends Potential with Regularization {
  def eVar   = entityEdge.n.variable.asVector
  def r1Var  = relation1Edge.n.variable.asVector
  def r2Var  = relation2Edge.n.variable.asVector
  val eMsgs  = entityEdge.msgs.asVector
  val r1Msgs = relation1Edge.msgs.asVector
  val r2Msgs = relation2Edge.msgs.asVector

  def sig(x: Double) = 1.0 / (1.0 + math.exp(-x))

  private def innerLossAndDirection(s: Double): (Double, Int) =
    if (target >= s) (1 + s - target, 1)
    else (1 + target - s, -1)

  override def valueForCurrentSetting(): Double = {
    val eVec = eVar.setting
    val r1Vec = r1Var.setting
    val r2Vec = r2Var.setting
    val r1p = sig(eVec dot r1Vec)
    val r2p = sig(eVec dot r2Vec)

    val s = r1p * (r2p - 1) + 1

    val p = innerLossAndDirection(s)._1
    math.log(p) + regLoss(eVec) + regLoss(r1Vec) + regLoss(r2Vec)
  }

  override def valueAndGradientForAllEdges(): Double = {
    val r1e = sig(eMsgs.n2f dot r1Msgs.n2f)
    val r2e = sig(eMsgs.n2f dot r2Msgs.n2f)

    val s = r1e * (r2e - 1) + 1
    
    val (p, dir) = innerLossAndDirection(s)

    val δr1e_r1 = eMsgs.n2f * r1e * (1 - r1e)
    val δr1e_e = r1Msgs.n2f * r1e * (1 - r1e)
    val δr2e_r2 = eMsgs.n2f * r2e * (1 - r2e)
    val δr2e_e = r2Msgs.n2f * r2e * (1 - r2e)

    r1Msgs.f2n = ((δr1e_r1 * (r2e - 1)) * (1.0 - p) * dir) + regGradient(r1Msgs.n2f)
    r2Msgs.f2n = ((δr2e_r2 * r1e) * (1.0 - p) * dir) + regGradient(r2Msgs.n2f)
    eMsgs.f2n = ((δr2e_e * r1e + δr1e_e * (r2e - 1)) * (1.0 - p) * dir) + regGradient(eMsgs.n2f)

    math.log(p) + regLoss(eMsgs.n2f) + regLoss(r1Msgs.n2f) + regLoss(r2Msgs.n2f)
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