package ml.wolfe.apps.factorization.io

import java.io.{FileWriter, File}

import cc.factorie.optimize._
import ml.wolfe.GradientBasedOptimizer
import ml.wolfe.apps.factorization._
import ml.wolfe.fg.{L2Regularization, CellLogisticLoss, VectorMsgs}
import ml.wolfe.util._
import ml.wolfe.{DenseVector, GradientBasedOptimizer, Wolfe}

import scala.io.Source
import scala.util.Random


/**
 * Created by Ingolf on 06/11/2014.
 */
object OptimiseHyperParameters extends App {

  val mfp = new MatrixFactorisationProblem()
  mfp.evaluate(mfp.startingValues)

}


class MatrixFactorisationProblem extends OptimisationProblem {
  override val parametersToOptimize: Seq[HyperParameter] = Seq(HyperParameter("lambda"), HyperParameter("alpha"))
  val startingValues = Map[String, Double]("lambda" -> 0.01, "alpha" -> 0.1)

  /**
   * Evaluate the optimisation problem given the set of hyper parameters.
   * @param hyperparameters The map of hyper parameters
   * @return The score of this evaluation, higher is better
   */
  override def evaluate(hyperparameters: Map[String, Double]): Double = {

    val debug = false //whether to use sampled matrices or the NAACL data
    val loadFormulae = debug && true //whether forumlae should be sampled for debugging
    //val print = false //whether to print the matrix (only do this for small ones!)

    val confPath = "conf/mf.conf"
    Conf.add(confPath)
    Conf.outDir //sets up output directory
    implicit val conf = Conf
    println("Using " + confPath)

    val outputPath = conf.getString("outDir")
    val fileName = conf.getString("mf.outFile")

    val k = conf.getInt("mf.k")

    // Get hyperparameters for this iteration
    val lambda = hyperparameters("lambda")
    val alpha = hyperparameters("alpha")

    val maxIter = conf.getInt("mf.maxIter")

    // val subsample = conf.getDouble("mf.subsample")
    val subsample = 0.1
    val negPerPos = conf.getInt("mf.negPerPos")
    val unobservedPerF = conf.getInt("mf.unobservedPerF")

    val cellWeight = conf.getDouble("mf.cellWeight")
    val formulaeWeight = conf.getDouble("mf.formulaeWeight")

    val optimizer = conf.getString("mf.optimizer")
    val batchTraining = conf.getBoolean("mf.batchTraining")


    val db = if (debug) {
      val tmp = new TensorKB(k)
      tmp.sampleTensor(10, 10, 0, 0.1) //samples a matrix
      if (loadFormulae) {
        tmp += Impl("r3", "r4")
        tmp += ImplNeg("r8", "r6")
      }
      tmp
    } else LoadNAACL(k, subsample)

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


    //fact factors
    for (d <- data) {
      val (colIx, rowIx, _) = d.key
      val a = A(rowIx) //entity
      val v = V(colIx) //relation

      fg.buildFactor(Seq(a, v))(_ map (_ => new VectorMsgs)) {
        e => new CellLogisticLoss(e(0), e(1), 1.0, lambda, cellWeight) with L2Regularization
      }

      (0 until negPerPos).foreach { i =>
        fg.buildStochasticFactor(Seq(v, db.sampleNode(colIx)))(_ map (_ => new VectorMsgs)) {
          e => new CellLogisticLoss(e(0), e(1), 0.0, lambda, cellWeight / negPerPos) with L2Regularization
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

    println("Optimizing...")
    Timer.time("optimization") {
      GradientBasedOptimizer(fg,
        if (batchTraining) new BatchTrainer(_, gradientOptimizer, maxIter) with ProgressLogging
        else new OnlineTrainer(_, gradientOptimizer, maxIter, fg.factors.size - 1) with ProgressLogging
      )
    }
    println("Done after " + Timer.reportedVerbose("optimization"))

    if (debug) {
      println("train:")
      println(db.toVerboseString(showTrain = true))
      println()

      println("predicted:")
      println(db.toVerboseString())
    } else {
      Conf.createSymbolicLinkToLatest() //rewire symbolic link to latest (in case it got overwritten)
      val pathToPredict = Conf.outDir.getAbsolutePath + "/" + fileName
      WriteNAACL(db, pathToPredict)
      EvaluateNAACL.main(Array("./conf/eval.conf", pathToPredict))

      import scala.sys.process._
      try {
        Process("pdflatex -interaction nonstopmode -shell-escape table.tex", new File(Conf.outDir.getAbsolutePath)).!!
      } catch {
        case e : Exception => println("Could not call pdftlatex on the latex: " + e.getMessage)
      }
      if (Conf.hasPath("formulaeFile") && Conf.getString("formulaeFile") != "None") {
        val formulaeFile = new File(Conf.getString("formulaeFile"))
        val lines = Source.fromFile(formulaeFile).getLines()
        val writer = new FileWriter(Conf.outDir.getAbsolutePath + "/" + formulaeFile.getAbsolutePath.split("/").last)
        writer.write(lines.mkString("\n"))
        writer.close()
      }
    }

    return 0

  }
}
