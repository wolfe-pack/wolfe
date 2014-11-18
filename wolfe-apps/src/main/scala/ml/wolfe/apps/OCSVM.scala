package ml.wolfe.apps

import breeze.linalg._
import breeze.numerics._
import cc.factorie.la._
import cc.factorie.optimize.{ConstantLearningRate, AdaGrad, OnlineTrainer}
import ml.wolfe._
import ml.wolfe.apps.factorization.MatrixFactorization._
import ml.wolfe.apps.factorization._
import ml.wolfe.apps.factorization.io.LoadNAACL
import ml.wolfe.fg.{L2Regularization, CellLogisticLoss, Potential, VectorMsgs}
import ml.wolfe.util.{PotentialDebugger, Conf}

import scala.util.Random

//rockt: could rho be a doublevar instead of a vectorvar?
//rockt: might be necessary to put l2 regularization into this factor as well
//fixme: get rid of idx, since it is only used for debugging
class OneClassSVMPerInstanceLoss(weightsEdge: FactorGraph.Edge, rhoEdge: FactorGraph.Edge,
                                 val xs: FactorieVector, val y: Double, val nu: Double, val idx: Int = 0) extends Potential {


  override def valueForCurrentSetting(): Double = {
    val currentWeights = weightsEdge.n.variable.asVector.setting
    val currentRho = rhoEdge.n.variable.asVector.setting(0)

    //println(currentWeights)
    //println(currentRho)

    -math.max((1 - nu) * currentRho - (currentWeights dot xs) * y, -nu * currentRho)
  }

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
    rhoEdge.msgs.asVector.f2n = new DenseTensor1(Array(
      if (currentRho >= (currentWeights dot xs) * y) -1 + nu
      else nu
    ))

    //    println(currentRho)
    //    if(idx==1 && currentRho < (currentWeights dot xs) * y ) {
    //      println("omgzz")
    //    }
    // Return value
    -math.max((1 - nu) * currentRho - (currentWeights dot xs) * y, -nu * currentRho)
  }
}

//rockt: this guy should have a vectorized implementation!
class l2reg(weightsEdge: FactorGraph.Edge, nu: Double, m: Int) extends Potential {
  override def valueForCurrentSetting(): Double = {
    val currentWeights = weightsEdge.n.variable.asVector.setting

    -(currentWeights dot currentWeights) * nu * m * 0.5
  }


  override def valueAndGradientForAllEdges() = {
    val currentWeights = weightsEdge.msgs.asVector.n2f

    // Set gradient for w.w
    weightsEdge.msgs.asVector.f2n = new DenseTensor1(currentWeights.length)
    for (j <- 0 until currentWeights.length) {
      weightsEdge.msgs.asVector.f2n(j) = -nu * m * currentWeights(j)
    }
    -(currentWeights dot currentWeights) * nu * m * 0.5
  }
}

class lowRankreg(weightsEdges: Seq[FactorGraph.Edge], epsilon: Double, gamma: Double) extends Potential {

  lazy val R = weightsEdges(0).msgs.asVector.n2f.length


  override def valueForCurrentSetting(): Double = ???


  override def valueAndGradientForAllEdges() = {
    val W = DenseMatrix.zeros[Double](R, R)

    //rockt: might be vectorizable as well
    for (j <- 0 until R) {
      for (i <- 0 until R)
      W(i, j) = weightsEdges(j).msgs.asVector.n2f(i)
    }

    val A: DenseMatrix[Double] = W * W.t
    val M = A + DenseMatrix.eye[Double](R) * epsilon

    val Minv = inv(M) //todo: let's see what happens

    val (lambdas, _, v) = eig(M)

    for (i <- 0 until R) {
      lambdas(i) = math.sqrt(lambdas(i))
    }
    val temp: DenseMatrix[Double] = v * diag(lambdas)
    val rootM: DenseMatrix[Double] = temp * v.t
    val D: DenseMatrix[Double] = rootM / trace(rootM)

    //Set gradient for: w dot (inv(D)w)
    for (i <- 0 until R) {
      weightsEdges(i).msgs.asVector.f2n = new DenseTensor1(R)
    }

    for (j <- 0 until R) {
      val w = W(::, j)
      val temp1 = D \ w
      for (k <- 0 until R) {
        weightsEdges(j).msgs.asVector.f2n(k) = -temp1(k) * gamma
      }
    }

    //for the loss need to sum these over R
    (for (t <- 0 until R) yield {
      val w = W(::, t)
      val temp3 = w.t * gamma
      val temp4 = D \ w
      temp3 * temp4
    }).sum
  }
}

case class XY(xs: Seq[FactorieVector], y: FactorieVector)

case class XYNew(xs: Tensor2, y: FactorieVector)

object OCSVM extends App {
  val nu = 0.5
  val gamma = 0 //low-rank regularization
  val epsilon = 1e-9

/*  def tens(vals: Double*) = new DenseTensor1(vals.toArray)

  val debug = true //whether to use sampled matrices or the NAACL data
  val loadFormulae = debug && true //whether forumlae should be sampled for debugging
  //val print = false //whether to print the matrix (only do this for small ones!)


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



  val matrix = new TensorKB()
  matrix += Cell("r0", "e0")
  matrix += Cell("r2", "e0")

  matrix += Cell("r0", "e1")
  matrix += Cell("r1", "e1")

  matrix += Cell("r1", "e2")
  matrix += Cell("r2", "e2")

  matrix += Cell("r0", "e3")
  matrix += Cell("r2", "e3")


  matrix.toFactorGraph

  println(matrix.toVerboseString())


  def dbToXYNew(db: TensorKB): Seq[XYNew] = {
    for (r <- db.relations) yield {
      val matrix = new SparseBinaryTensor2(db.dim2, db.dim1)
      val y = new SparseTensor1(db.dim2)

      db.getBy1(r).foreach(t => {
        val tCurrentIx = db.cellIxToIntIx2(t._1)
        y.update(tCurrentIx, 1.0)

        db.getBy2(t._1).filterNot(_._1 == r).foreach(rNew => {
          val rIx = db.cellIxToIntIx1(rNew._1)
          matrix.update(tCurrentIx, rIx, 1.0)
        })
      })

      XYNew(matrix, y)
    }
  }

  val data = dbToXYNew(matrix)


  import PimpMyFactorie._

  /*
  for (datum <- data) {
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


  val R           = data.length
  val fg          = new FactorGraph()
  val weightnodes = for (i <- 0 until R) yield fg.addVectorNode(R, "weight" + i)
  val rhonodes    = for (i <- 0 until R) yield fg.addVectorNode(1, "rho" + i)

  val rand = new Random(0l)

  rhonodes.foreach(rho => {
    val rhoValue = new DenseTensor1(Array(rand.nextGaussian()))
    rho.variable.asVector.b = rhoValue
    rho.variable.asVector.setting = rhoValue
  })

  weightnodes.foreach(w => {
    val wValue = new DenseTensor1((0 until R).map(i => rand.nextGaussian()).toArray)
    w.variable.asVector.b = wValue
    w.variable.asVector.setting = wValue
  })

  //rockt: instead of fg.addVectorNode() for rho, could we use fg.addContinuousNode()?


  for (j <- 0 until R) {
    val m = data(j).xs.dim1
    for (i <- 0 until m) {
      fg.buildFactor(Seq(weightnodes(j), rhonodes(j)))(_ => Seq(new VectorMsgs, new VectorMsgs))(
        edges => new OneClassSVMPerInstanceLoss(edges(0), edges(1), data(j).xs.getSparseRow(i), data(j).y(i), nu, j)
      )
    }
    //fg.buildFactor(Seq(weightnodes(j)))(_ => Seq(new VectorMsgs))(edges => new l2reg(edges(0), nu, m))
  }


  //fg.buildFactor(weightnodes) (edges => for(_ <- edges) yield new VectorMsgs) (edges => new lowRankreg(edges, epsilon, gamma))

  D3Implicits.saveD3Graph(fg)

  fg.build()



  GradientBasedOptimizer(fg,
    new OnlineTrainer(_, new AdaGrad(rate = 0.1), 1000, fg.factors.size - 1)
    //new OnlineTrainer(_, new ConstantLearningRate(baseRate = 0.05), 1000, fg.factors.size - 1)
  )

  //fg.factors.map(_.potential).foreach(PotentialDebugger.checkGradients(_, debug = true))


  for (w <- weightnodes) {
    println(w.variable.label + "\t" + w.variable.asVector.b)
  }

  for (rho <- rhonodes) {
    println(rho.variable.label + "\t" + rho.variable.asVector.b)
  }


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

      val ts = matrix.getBy2(tuple).map(t => matrix.cellIxToIntIx1(t._1)).foreach(ix => {
        x.update(ix, 1.0)
      })
      x.update(col, 0.0)

      /*
      println(wj.size)
      println(wj.toPrettyString)
      println(x.size)
      println(x.toPrettyString)
      */

      print("%-10.5f\t".format((x dot wj) - rhoj))
      //print("%-10.5f\t".format(x dot wj))
    }
    println()
  }

  println(matrix.toVerboseString())

}

object OCSVMGradientChecking extends App {
  val fg = new FactorGraph()
  val n1 = fg.addVectorNode(2, "w1")
  val n2 = fg.addVectorNode(1, "rho1")
  val n3 = fg.addVectorNode(2, "w2")
  val n4 = fg.addVectorNode(1, "rho2")

  val nu = 0.5

  val data = new SparseBinaryTensor1(2)
  data.update(0, 1.0)
  /*
  data.update(10, 1.0)
  data.update(45, 1.0)
  data.update(70, 1.0)
  */

  val data2 = new SparseBinaryTensor1(2)
  data.update(1, 1.0)
  /*
  data.update(5, 1.0)
  data.update(50, 1.0)
  data.update(80, 1.0)
  */


  fg.buildFactor(Seq(n1, n2))(_ => Seq(new VectorMsgs, new VectorMsgs))(
    edges => new OneClassSVMPerInstanceLoss(edges(0), edges(1), data, 1.0, nu)
  )

  fg.buildFactor(Seq(n3, n4))(_ => Seq(new VectorMsgs, new VectorMsgs))(
    edges => new OneClassSVMPerInstanceLoss(edges(0), edges(1), data2, 1.0, nu)
  )

  fg.buildFactor(Seq(n1))(_ => Seq(new VectorMsgs))(edges => new l2reg(edges(0), nu, 2))


  //fg.buildFactor(Seq(n1,n3)) (edges => for(_ <- edges) yield new VectorMsgs) (edges => new lowRankreg(edges, 0.00001, 0.1))

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

