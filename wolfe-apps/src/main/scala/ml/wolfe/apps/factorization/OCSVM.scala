package ml.wolfe.apps.factorization

import java.io.{File, FileWriter}

import breeze.linalg._
import cc.factorie.la._
import cc.factorie.optimize.{AdaGrad, OnlineTrainer}
import ml.wolfe._
import ml.wolfe.apps.factorization.io.{EvaluateNAACL, LoadNAACL}

//import ml.wolfe.apps.factorization.io.{EvaluateNAACL, LoadNAACL}
import ml.wolfe.fg._
import ml.wolfe.util._

import scala.io.Source
import scala.util.Random


object OCSVMImplicits {
  implicit def breezeToFactorieVector(breezeVector: breeze.linalg.DenseVector[Double]): FactorieVector =
    new DenseTensor1(breezeVector.toArray)

  implicit class ElementWiseVectorMul(self: Tensor1) {
    def :* (other: Tensor1): Tensor1 = other match {
      case s: SparseIndexedTensor1 => {
        val newSelf = self.copy

        s.activeElements.foreach(elem => {
          val (ix, value) = elem
          newSelf.update(ix, value * self(ix))
        })
        newSelf
      }
    }
  }
}


//rockt: could rho be a doublevar instead of a vectorvar?
//rockt: might be necessary to put l2 regularization into this factor as well
//fixme: get rid of idx, since it is only used for debugging
class OneClassSVMPerInstanceLoss(weightsEdge: FactorGraph.Edge, rhoEdge: FactorGraph.Edge,
                                 val x: FactorieVector, val y: Double, val nu: Double, mask: FactorieVector) extends Potential {
  override def valueForCurrentSetting(): Double = {
    val w = weightsEdge.n.variable.asVector.setting    
    val rho = rhoEdge.n.variable.asVector.setting(0)
    -math.max((1 - nu) * rho - (w dot x) * y, -nu * rho) - (nu/2.0) * w.twoNormSquared
  }

  override def valueAndGradientForAllEdges() = {
    import OCSVMImplicits._

    val w = weightsEdge.msgs.asVector.n2f

    //assert(x.activeElements.map(_._1).toSet subsetOf mask.activeElements.map(_._1).toSet)

    val rho = rhoEdge.msgs.asVector.n2f(0)

    if (rho >= (w dot x)) {
      //weightsEdge.msgs.asVector.f2n = (w * -nu) + x

      /*
      println("x:                    " + x.toString())
      println("m:                    " + mask.toString())
      println("m * -nu:              " + (mask * -nu).toString())
      println("w:                    " + w.toString())
      println("w * (mask * -nu):     " + (w :* (mask * -nu)).toString())
      println("w * (mask * -nu) + x: " + ((w :* (mask * -nu)) + x).toString())
      println()
      */

      weightsEdge.msgs.asVector.f2n = (w :* (mask * -nu)) + x

      rhoEdge.msgs.asVector.f2n = new DenseTensor1(Array(nu - 1))
    } else {
      //weightsEdge.msgs.asVector.f2n = w * -nu
      weightsEdge.msgs.asVector.f2n = w :* (mask * -nu)
      rhoEdge.msgs.asVector.f2n = new DenseTensor1(Array(nu))
    }

    -math.max((1 - nu) * rho - (w dot x) * y, -nu * rho) - (nu/2.0) * w.twoNormSquared
  }
}

class LowRankRegularizer(weightsEdges: Seq[FactorGraph.Edge], epsilon: Double, gamma: Double) extends Potential {
  import OCSVMImplicits._

  lazy val numClassifiers = weightsEdges.size
  lazy val numWeights = weightsEdges(0).msgs.asVector.n2f.length


  override def valueForCurrentSetting(): Double = {
    val W = DenseMatrix.zeros[Double](numWeights, numClassifiers)

    //rockt: might be vectorizable as well
    //building W, slow!
    for (col <- 0 until numClassifiers) {
      for (row <- 0 until numWeights)
        W(row, col) = weightsEdges(col).n.variable.asVector.setting(row) //.msgs.asVector.n2f(row)
    }

    //numWeights × numWeights matrix
    val A: DenseMatrix[Double] = W * W.t

    //WW^T + εI
    val M = A + DenseMatrix.eye[Double](numWeights) * epsilon

    //(WW^T + εI)^(1/2)
    val (lambdas, _, v) = eig(M) //fixme: runtime bottleneck
    //val (lambdas, v) = eigSym(M) //fixme: runtime bottleneck
    for (i <- 0 until numWeights) {
      lambdas(i) = math.sqrt(lambdas(i))
    }
    val temp: DenseMatrix[Double] = v * diag(lambdas)
    val rootM: DenseMatrix[Double] = temp * v.t

    //(WW^T + εI)^(1/2) / tr((WW^T + εI)^(1/2))
    //fixme
    val D: DenseMatrix[Double] = rootM / trace(rootM)
    //val D: DenseMatrix[Double] = (DenseMatrix.eye[Double](numWeights)) :* 3.0 //sanity check

    (for (i <- 0 until numClassifiers) yield {
      val w = W(::, i)
      val temp1 = D \ w

      //weightsEdges(i).msgs.asVector.f2n = -temp1 * (gamma / 2.0)

      w.t * gamma * -temp1
    }).sum
  }



  override def valueAndGradientForAllEdges() = {
    val W = DenseMatrix.zeros[Double](numWeights, numClassifiers)

    //rockt: might be vectorizable as well
    //building W, slow!
    for (col <- 0 until numClassifiers) {
      for (row <- 0 until numWeights)
      W(row, col) = weightsEdges(col).msgs.asVector.n2f(row)
    }

    //numWeights × numWeights matrix
    val A: DenseMatrix[Double] = W * W.t

    //WW^T + εI
    val M = A + DenseMatrix.eye[Double](numWeights) * epsilon

    //(WW^T + εI)^(1/2)
    val (lambdas, _, v) = eig(M) //fixme: runtime bottleneck
    //val (lambdas, v) = eigSym(M) //fixme: runtime bottleneck
    for (i <- 0 until numWeights) {
      lambdas(i) = math.sqrt(lambdas(i))
    }
    val temp: DenseMatrix[Double] = v * diag(lambdas)
    val rootM: DenseMatrix[Double] = temp * v.t

    /*
    val TMP = cholesky(M)
    val (u, s, _) = svd(TMP)
    val rootM = (u * s) * u.t
    */

    //(WW^T + εI)^(1/2) / tr((WW^T + εI)^(1/2))
    //fixme
    val D: DenseMatrix[Double] = rootM / trace(rootM)
    //val D: DenseMatrix[Double] = (DenseMatrix.eye[Double](numWeights)) :* 3.0 //sanity check


    (for (i <- 0 until numClassifiers) yield {
      val w = W(::, i)
      val temp1 = D \ w

      weightsEdges(i).msgs.asVector.f2n = -temp1 * (gamma * 2)

      w.t * gamma * -temp1
    }).sum
  }
}

case class XY(xs: Seq[FactorieVector], y: FactorieVector)

case class XYNew(xs: Tensor2, y: FactorieVector, relation: Any, featureMask: FactorieVector)

object OCSVM extends App {
  import OCSVMImplicits._
  import ml.wolfe.apps.PimpMyFactorie._

  val confPath = args.lift(0).getOrElse("conf/mf-oc.conf")
  Conf.add(confPath)
  Conf.outDir //sets up output directory
  implicit val conf = Conf
  println("Using " + confPath)

  val nu = conf.getDouble("mf.lambda")
  //val nu = 0.00001

  val k = 0
  val subsample = 1.0
  val alpha = conf.getDouble("mf.alpha")
  val maxIter = conf.getInt("mf.maxIter")

  val gamma = conf.getDouble("mf.gamma") //low-rank regularization
  val epsilon = 1e-9

  val fileName = conf.getString("mf.outFile")


  val debug = conf.getBoolean("mf.debug") //whether to use sampled matrices or the NAACL data
  //val loadFormulae = debug && true //whether forumlae should be sampled for debugging
  //val print = false //whether to print the matrix (only do this for small ones!)


  /*
  val k = 100
  val subsample = 1.0

  println("Loading NAACL data...")
  val matrix = if (debug) {
    val tmp = new TensorKB()
    tmp.sampleTensor(10, 10, 0, 0.4) //samples a matrix
    if (loadFormulae) {
      tmp += Impl("r3", "r4")
      tmp += ImplNeg("r8", "r6")
    }
    tmp
  } else LoadNAACL(k, subsample)


  */

  val matrix =
      if (debug) {
      val tmp = new TensorKB()
      //tmp.sampleTensor(6, 4, 0, 0.5) //samples a matrix
      tmp.sampleTensor(10, 10, 0, 0.25) //samples a matrix
      tmp.toFactorGraph
      println(tmp.toVerboseString(true))


    /*
  matrix += Cell("r1", "e2")
  matrix += Cell("r2", "e2")
  matrix += Cell("r3", "e2")
  matrix += Cell("r4", "e2")
  matrix += Cell("r5", "e2")
  //matrix += Cell("r2", "e1")
  matrix += Cell("r3", "e1")
  matrix += Cell("r3", "e3")
  matrix += Cell("r3", "e4")
  matrix += Cell("r4", "e4")
  */

    /*
  val matrix = new TensorKB()

  (0 until 6).foreach(i => matrix += Cell("r"+i, "e"+i))
  matrix += Cell("r5", "e0")
  matrix += Cell("r3", "e1")
  */

    /*
  //matrix += Cell("r0", "e0")
  matrix += Cell("r2", "e0")

  matrix += Cell("r0", "e1")
  matrix += Cell("r1", "e1")

  matrix += Cell("r1", "e2")
  matrix += Cell("r2", "e2")

  matrix += Cell("r0", "e3")
  matrix += Cell("r2", "e3")
  */

    /*
  matrix += Cell("r0", "e0")
  matrix += Cell("r0", "e1")
  matrix += Cell("r0", "e2")
  matrix += Cell("r0", "e3")

  matrix += Cell("r1", "e0")
  matrix += Cell("r1", "e1")
  matrix += Cell("r1", "e2")

  //matrix += Cell("r2", "e3")
  matrix += Cell("r2", "e3")
  */
    tmp
  } else LoadNAACL(k, subsample)


  val numCols = matrix.keys1.size
  val numRows = matrix.keys2.size

  def dbToXYNew(db: TensorKB, relFilter: Any => Boolean = rel => true): Seq[XYNew] = {
    for (r <- db.relations; if relFilter(r)) yield {
      val matrix = new SparseBinaryTensor2(db.dim2, db.dim1)
      val y = new SparseBinaryTensor1(db.dim2)
      val featureMask = new SparseTensor1(db.dim1) //all indices of features we have seen for this relation
      
      val progressBar = new ProgressBar(db.getBy1(r).size, 10)
      progressBar.start()

      db.getBy1(r).foreach(t => {
        val tCurrentIx = db.cellIxToIntIx2(t._1)
        y.update(tCurrentIx, 1.0)

        db.getBy2(t._1).filterNot(_._1 == r).foreach(rNew => {
          val rIx = db.cellIxToIntIx1(rNew._1)
          matrix.update(tCurrentIx, rIx, 1.0)
          featureMask.update(rIx, 1.0)
        })

        progressBar(t.toString())
      })

      /*
      println(r)
      println(matrix.toPrettyString)
      println(featureMask.toPrettyString)
      println()
      */

      XYNew(matrix, y, r, featureMask)
    }
  }

  println(matrix.toInfoString)


  println("Generating training data...")

  lazy val testRelations = Seq(
    "person/company",
    "location/containedby",
    "person/nationality"/*,
    "author/works_written",
    "parent/child",
    "person/place_of_birth",
    "person/place_of_death",
    "neighborhood/neighborhood_of",
    "person/parents",
    "company/founders",
    "sports_team/league",
    "team_owner/teams_owned",
    "team/arena_stadium",
    "film/directed_by",
    "roadcast/area_served",
    "structure/architect",
    "composer/compositions",
    "person/religion",
    "film/produced_by" */
  ).toSet

  def testRelationFilter(r: Any) = testRelations.exists(s => r.toString.contains(s))
  //def testRelationFilter(r: Any) = matrix.getBy1(r).size < 5
  //def testRelationFilter(r: Any) = true


  val data = dbToXYNew(matrix, r => debug || testRelationFilter(r))

  println("Training " + data.size + " classifiers...")


  /*
  for ((datum, ix) <- data.zipWithIndex) {
    println(ix)
    val matrix = datum.xs
    val y = datum.y

    println()
    println(matrix.toPrettyString)
    println()
    println(y.toPrettyString)
    println()
  }
  */



  /*
  println("y:\t" + y.mkString("\t"))
  println()
  (0 until x.dim1).foreach(row => {
    (0 until x.dim2).foreach(col => print(x(row, col) + "\t"))
    println()
  })

  println()

  println(x.getSparseRow(0).mkString("\t"))
  println(x.getSparseRow(1).mkString("\t"))
  println(x.getSparseRow(2).mkString("\t"))
  println(x.getSparseRow(3).mkString("\t"))
  */


  println("Building factor graph...")

  val R           = data.length
  val ixToRelationMap = (0 until R).map(ix => ix -> data(ix).relation).toMap

  val fg          = new FactorGraph()
  val weightnodes = for (i <- 0 until R) yield fg.addVectorNode(R, "weight" + i)
  val rhonodes    = for (i <- 0 until R) yield fg.addVectorNode(1, "rho" + i)

  val rand = new Random(0l)

  rhonodes.foreach(rho => {
    val rhoValue = new DenseTensor1(Array(rand.nextGaussian() * 0.1))
    //val rhoValue = new DenseTensor1(Array(0.0))
    rho.variable.asVector.b = rhoValue
    rho.variable.asVector.setting = rhoValue
  })

  weightnodes.zipWithIndex.foreach(t => {
    val (w, ix) = t

    val wValue = new DenseTensor1(numCols)

    data(ix).featureMask.activeElements.foreach(elem => {
      val fix = elem._1
      wValue.update(fix, rand.nextGaussian() * 0.1)
    })

    /*
    val wValue = new DenseTensor1((0 until numCols).map(i => {
      if (i != ix) rand.nextGaussian() * 0.1
      else 0.0
    }).toArray)
    */
    w.variable.asVector.b = wValue
    w.variable.asVector.setting = wValue
  })

  //rockt: instead of fg.addVectorNode() for rho, could we use fg.addContinuousNode()?


  for (j <- 0 until R) {
    val m = data(j).xs.dim1
    val y = data(j).y
    val mask = data(j).featureMask

    y.activeElements.foreach { case (ix, target) =>
      fg.buildFactor(Seq(weightnodes(j), rhonodes(j)))(_ => Seq(new VectorMsgs, new VectorMsgs))(
        edges => new OneClassSVMPerInstanceLoss(edges(0), edges(1), data(j).xs.getSparseRow(ix), target, nu, mask)
      )
    }
  }

  if (gamma != 0)
    fg.buildFactor(weightnodes) (edges => for(_ <- edges) yield new VectorMsgs) (edges => new LowRankRegularizer(edges, epsilon, gamma))

  //D3Implicits.saveD3Graph(fg)

  fg.build()

  println(fg.toInspectionString)


  println("Optimizing...")

  GradientBasedOptimizer(fg,
    new OnlineTrainer(_, new AdaGrad(rate = alpha), maxIter, fg.factors.size - 1) with ProgressLogging
    //new OnlineTrainer(_, new ConstantLearningRate(baseRate = 0.05), 1000, fg.factors.size - 1)
  )

  //fg.factors.map(_.potential).foreach(PotentialDebugger.checkGradients(_, debug = true))


  if (debug) {
    for (w <- weightnodes) {
      println(w.variable.label + "\t" + w.variable.asVector.b)
    }

    for (rho <- rhonodes) {
      println(rho.variable.label + "\t" + rho.variable.asVector.b)
    }



    println("indices:\n" + matrix.cellIxToIntIx1.mkString("\n"))

    print("\t")
    for (col <- 0 until matrix.dim1) print(matrix.keys1(col) + "\t")
    println()

    for (row <- 0 until matrix.dim2) {
      print(matrix.keys2(row) + "\t")
      for (col <- 0 until matrix.dim1) {
        val wj = weightnodes(col).variable.asVector.b
        val rhoj = rhonodes(col).variable.asVector.b(0)
        val r = matrix.keys1(col)

        val tuple = matrix.keys2(row)
        val x = new SparseTensor1(matrix.dim1)

        matrix.getBy2(tuple).map(t => matrix.cellIxToIntIx1(t._1)).foreach(ix => {
          if (col != ix)
            x.update(ix, 1.0)
        })
        //x.update(col, 0.0) //todo: why doesn't overwriting with 0 work in FACTORIE?

        /*
      println(s"col $col row $row")
      println("w:\n" + wj.toPrettyString)
      println("x:\n" + x.toPrettyString)
      println("rho:\t" + rhoj)
      println()
      */

        print("%-10.5f\t".format((wj dot x) - rhoj))
        //print("%-10.5f\t".format(wj dot x))
      }
      println()
    }


    println(matrix.toVerboseString(true))

    fg.factors.map(_.potential).foreach(PotentialDebugger.checkGradients(_, debug = true, sparseMode = true))
    //fg.factors.map(_.potential).foreach(PotentialDebugger.checkGradients(_, debug = false, sparseMode = true))

  } else {
    /*
    println(rhonodes.head.variable.asVector.b)
    println(weightnodes.head.variable.asVector.b.size)
    println(weightnodes.head.variable.asVector.b.toArray.mkString("\n"))
    */





    Conf.createSymbolicLinkToLatest() //rewire symbolic link to latest (in case it got overwritten)
    val pathToPredict = Conf.outDir.getAbsolutePath + "/" + fileName


    val predictions =
      (0 until R).flatMap(col => {
        val relation = ixToRelationMap(col)

        for {
          row <- 0 until matrix.dim2
          tuple = matrix.keys2(row)
          if matrix.getFact(relation, tuple, DefaultIx).exists(_.test)
          wj = weightnodes(col).variable.asVector.b
          rhoj = rhonodes(col).variable.asVector.b(0)
        } yield {

          val x = new SparseTensor1(matrix.dim1)
          matrix.getBy2(tuple).map(t => matrix.cellIxToIntIx1(t._1)).foreach(ix => {
            if (col != ix)
              x.update(ix, 1.0)
          })

          ((wj dot x) - rhoj, tuple, relation)
        }
      }).sortBy(-_._1)

    val fileWriter = new FileWriter(pathToPredict)
    predictions.foreach { case (p, es, r) =>
      val Array(e1,e2) = es.toString.tail.init.split(",")
      fileWriter.write(s"$p\t$e1|$e2\t$r\t$r\n")
    }

    fileWriter.close()


    EvaluateNAACL.main(Array("./conf/eval.conf", pathToPredict))

    import scala.sys.process._
    Process("pdflatex -interaction nonstopmode -shell-escape table.tex", new File(Conf.outDir.getAbsolutePath)).!!

    if (Conf.hasPath("formulaeFile") && Conf.getString("formulaeFile") != "None") {
      val formulaeFile = new File(Conf.getString("formulaeFile"))
      val lines = Source.fromFile(formulaeFile).getLines()
      val writer = new FileWriter(Conf.outDir.getAbsolutePath + "/" + formulaeFile.getAbsolutePath.split("/").last)
      writer.write(lines.mkString("\n"))
      writer.close()
    }

    //fg.factors.map(_.potential).foreach(PotentialDebugger.checkGradients(_, debug = true, sparseMode = true))
  }
}

object OCSVMGradientChecking extends App {


  val fg = new FactorGraph()
  val n1 = fg.addVectorNode(3, "w1")
  val n2 = fg.addVectorNode(1, "rho1")
  val n3 = fg.addVectorNode(3, "w2")
  val n4 = fg.addVectorNode(1, "rho2")
  val n5 = fg.addVectorNode(3, "w3")
  val n6 = fg.addVectorNode(1, "rho3")

  val nu = 0.5

  val data = new SparseBinaryTensor1(2)
  data.update(0, 1.0)


  val data2 = new SparseBinaryTensor1(2)
  data.update(1, 1.0)
  /*
  data.update(5, 1.0)
  data.update(50, 1.0)
  data.update(80, 1.0)
  */


  /*
  fg.buildFactor(Seq(n1, n2))(_ => Seq(new VectorMsgs, new VectorMsgs))(
    edges => new OneClassSVMPerInstanceLoss(edges(0), edges(1), data, 1.0, nu)
  )

  fg.buildFactor(Seq(n3, n4))(_ => Seq(new VectorMsgs, new VectorMsgs))(
    edges => new OneClassSVMPerInstanceLoss(edges(0), edges(1), data2, 1.0, nu)
  )
  */


  fg.buildFactor(Seq(n1,n3)) (edges => for(_ <- edges) yield new VectorMsgs) (edges => new LowRankRegularizer(edges, 0.00001, 0.1))



  fg.build()


  val rho1 = new DenseTensor1(Array(10.0))
  n2.variable.asVector.b = rho1
  n2.variable.asVector.setting = rho1
  val rho2 = new DenseTensor1(Array(5.0))
  n4.variable.asVector.b = rho2
  n4.variable.asVector.setting = rho2

  //initializing weights and messages
  val rand = new Random(0l)
  def nextInit() = rand.nextGaussian() * 0.1
  Seq(n1, n3).foreach(n => {
    val vec = new DenseTensor1((0 until 2).map(i => nextInit()).toArray)
    n.variable.asVector.b = vec
    n.variable.asVector.setting = vec
  })
  fg.factors.foreach(_.edges.foreach(e => e.msgs.asVector.n2f = e.n.variable.asVector.b))

  //gradient checking
  fg.factors.map(_.potential).foreach(PotentialDebugger.checkGradients(_, debug = true))
}

object ElemMulTest extends App {
  import OCSVMImplicits._
  import ml.wolfe.apps.PimpMyFactorie._
  
  val w = new DenseTensor1(Array(1.0, 2.0, 3.0, 4.0))
  val x = new SparseTensor1(w.dim1)
  x.update(1, 3.0)
  x.update(3, 5.0)

  println((w :* x).toPrettyString)
  println((w :* x).toPrettyString)
}