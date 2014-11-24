package ml.wolfe.apps.factorization

import java.io.{File, FileWriter}

import cc.factorie.optimize._
import ml.wolfe.apps.factorization.io._
import ml.wolfe.fg.{L2Regularization, _}
import ml.wolfe.util.{Conf, ProgressLogging, Timer}
import ml.wolfe.{DenseVector, GradientBasedOptimizer, Wolfe}

import scala.io.Source
import scala.util.Random

/**
 * @author Sebastian Riedel
 * @author rockt
 */

object MatrixFactorization extends App {
  val mf = new MatrixFactorization(args.lift(0).getOrElse("conf/mf.conf"))
  val wMAP = mf.run()
  println(wMAP)
}

class MatrixFactorization(confPath: String = "conf/mf.conf") {
  val debug = false //whether to use a small synthetic matrix or actual data
  val loadFormulae = debug && true //whether forumlae should be sampled for debugging
  //val print = false //whether to print the matrix (only do this for small ones!)

  Conf.add(confPath)
  Conf.outDir //sets up output directory
  implicit val conf = Conf
  println("Using " + confPath)

  val dataType = conf.getString("dataType")
  assert(dataType == "naacl" || dataType == "figer", s"dataType $dataType should be 'naacl' or 'figer'.")
  val useFeatures = dataType == "figer" && conf.getBoolean("figer.use-features")

  val outputPath = conf.getString("outDir")
  val fileName = conf.getString("mf.outFile")

  val k = conf.getInt("mf.k")
  val lambda = conf.getDouble("mf.lambda")
  val alpha = conf.getDouble("mf.alpha")
  val maxIter = conf.getInt("mf.maxIter")

  val subsample = conf.getDouble("mf.subsample")
  val negPerPos = conf.getInt("mf.negPerPos")
  val unobservedPerF = conf.getInt("mf.unobservedPerF")

  val cellWeight = conf.getDouble("mf.cellWeight")
  val formulaeWeight = conf.getDouble("mf.formulaeWeight")

  val optimizer = conf.getString("mf.optimizer")
  val batchTraining = conf.getBoolean("mf.batchTraining")

  val bpr = conf.getBoolean("mf.bpr")

  val db = if (debug) {
    val tmp = new TensorKB(k)
    tmp.sampleTensor(10, 10, 0, 0.1) //samples a matrix
    if (loadFormulae) {
      tmp += Impl("r3", "r4")
      tmp += ImplNeg("r8", "r6")
    }
    tmp
  } else dataType match {
    case "naacl" => LoadNAACL(k, subsample)
    case "figer" => LoadFIGER(k, subsample)
  }

  val rand = new Random(0l)

  val fg = db.toFactorGraph
  val data = rand.shuffle(db.trainCells)
  val V = db.ix1ToNodeMap //cols
  val A = db.ix2ToNodeMap //rows

  //initialize embeddings
  //def nextInit() = (rand.nextDouble() - 0.5) * 0.1
  def nextInit() = rand.nextGaussian() * 0.1
  (V.values.view ++ A.values.view).foreach(n =>
    n.variable.asVector.b = new DenseVector((0 until k).map(i => nextInit()).toArray))
  if(useFeatures) db match {
    case f: Features => f.fnodes.foreach(n =>
      n.variable.asVector.b = new DenseVector((0 until k).map(i => nextInit()).toArray))
  }


  //fact factors
  for (d <- data) {
    val (colIx, rowIx, _) = d.key
    val a = A(rowIx) //entity
    val v = V(colIx) //relation

    if (bpr) fg.buildStochasticFactor(Seq(a, db.sampleNode(colIx), v))(_ map (_ => new VectorMsgs)) {
      e => new BPRPotential(e(0), e(1), e(2), 1.0, lambda) with L2Regularization
    }
    else {
      fg.buildFactor(Seq(a, v))(_ map (_ => new VectorMsgs)) {
        e => new CellLogisticLoss(e(0), e(1), 1.0, lambda, cellWeight) with L2Regularization
      }

      (0 until negPerPos).foreach { i =>
        fg.buildStochasticFactor(Seq(v, db.sampleNode(colIx)))(_ map (_ => new VectorMsgs)) {
          e => new CellLogisticLoss(e(0), e(1), 0.0, lambda, cellWeight / negPerPos) with L2Regularization
        }
      }
    }
  }

  //formulae factors
  for (d <- data) {
    //colIx: relation
    //rowIx: entity
    val (colIx, rowIx, _) = d.key
    val a = A(rowIx)
    val v = V(colIx)

    for (formula <- db.formulaeByPredicate(colIx)) {
      val cNode = v
      if (formula.isFormula2) {
        val Seq(p1, p2) = formula.predicates

        //can only inject formulae whose predicates exist
        if (db.node1(p1).isDefined && db.node1(p2).isDefined) {
          val p1Node = db.node1(p1).get
          val p2Node = db.node1(p2).get

          formula match {
            case Impl(_, _, target) =>
              fg.buildFactor(Seq(cNode, p1Node, p2Node))(_ map (_ => new VectorMsgs)) {
                e => new ImplPotential(e(0), e(1), e(2), target, lambda, formulaeWeight) with L2Regularization
              }
              (0 until unobservedPerF).foreach { i =>
                fg.buildStochasticFactor(Seq(db.sampleNode(colIx), p1Node, p2Node))(_ map (_ => new VectorMsgs)) {
                  e => new ImplPotential(e(0), e(1), e(2), target, lambda, formulaeWeight) with L2Regularization
                }
              }

            case ImplNeg(_, _, target) =>
              fg.buildFactor(Seq(cNode, p1Node, p2Node))(_ map (_ => new VectorMsgs)) {
                e => new ImplNegPotential(e(0), e(1), e(2), target, lambda, formulaeWeight) with L2Regularization
              }
              (0 until unobservedPerF).foreach { i =>
                fg.buildStochasticFactor(Seq(db.sampleNode(colIx), p1Node, p2Node))(_ map (_ => new VectorMsgs)) {
                  e => new ImplNegPotential(e(0), e(1), e(2), target, lambda, formulaeWeight) with L2Regularization
                }
              }
          }
        }
      } else {
        ???
      }
    }

  }

  fg.build()


  println("DB:" + db.toInfoString)
  println("FG:" + fg.toInspectionString)

  val gradientOptimizer = optimizer match {
    case "SGD" => new ConstantLearningRate(baseRate = alpha)
    case "AdaGrad" => new AdaGrad(rate = alpha)
    case "AdaMira" => new AdaMira(rate = alpha)
    case "LBFGS" => new LBFGS(Double.MaxValue, Int.MaxValue) //rockt: not working atm
    case "AvgPerceptron" => new AveragedPerceptron()
  }


  def run(): Double = {
    println("Optimizing...")
    Timer.time("optimization") {
      GradientBasedOptimizer(fg,
        if (batchTraining) new BatchTrainer(_, gradientOptimizer, maxIter) with ProgressLogging
        else new OnlineTrainer(_, gradientOptimizer, maxIter, fg.factors.size - 1) with ProgressLogging
      )
    }
    println("Done after " + Timer.reportedVerbose("optimization"))

    var wMAP = 0.0

    if (debug) {
      println("train:")
      println(db.toVerboseString(showTrain = true))
      println()

      println("predicted:")
      println(db.toVerboseString())
    } else {
      Conf.createSymbolicLinkToLatest() //rewire symbolic link to latest (in case it got overwritten)
      val pathToPredict = Conf.outDir.getAbsolutePath + "/" + fileName
      dataType match {
        case "naacl" =>
          WriteNAACL(db, pathToPredict)
          wMAP = new EvaluateNAACL("./conf/eval.conf", pathToPredict).eval()
        case "figer" =>
          WriteFIGER(db, pathToPredict)
          EvaluateFIGER.main(Array(pathToPredict, Conf.outDir.getAbsolutePath))
      }

      db.writeVectors(Conf.outDir.getAbsolutePath + "/vectors.tsv")

      import scala.sys.process._
      Process("pdflatex -interaction nonstopmode -shell-escape table.tex", new File(Conf.outDir.getAbsolutePath)).!!

      if (Conf.hasPath(dataType + ".formulaeFile") && Conf.getString(dataType + ".formulaeFile") != "None") {
        val formulaeFile = new File(Conf.getString(dataType + ".formulaeFile"))
        val lines = Source.fromFile(formulaeFile).getLines()
        val writer = new FileWriter(Conf.outDir.getAbsolutePath + "/" + formulaeFile.getAbsolutePath.split("/").last)
        writer.write(lines.mkString("\n"))
        writer.close()
      }
    }

    wMAP
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