package ml.wolfe.apps.factorization

import cc.factorie.la.DenseTensor1
import cc.factorie.optimize.{AdaGrad, BatchTrainer}
import ml.wolfe.FactorGraph.Edge
import ml.wolfe.GradientBasedOptimizer
import ml.wolfe.fg._
import math._

import scala.collection.mutable

/**
 * @author Sebastian Riedel
 */
object GeometricMF extends App {
  val k  = 2
  val db = new TensorKB(k)
  db.sampleTensor(10, 10, 0, 0.1)
  //samples a matrix

  val fg   = db.toFactorGraph
  val data = db.cells
  val V    = db.ix1ToNodeMap
  //cols
  val A    = db.ix2ToNodeMap //rows

  //most of this will potentially go into TensorKB
  for (d <- data) {
    val (colIx, rowIx, _) = d.key
    val a = A(rowIx)
    val v = V(colIx)

    //create positive fact factor
    fg.buildFactor(Seq(a, v))(_ map (_ => new VectorMsgs)) {
      e => new CellLogisticLoss2(e(0), e(1), 1.0)
    }
  }

  //create repulsion factors
  for (relation1 <- 0 until db.relations.length; relation2 <- relation1 + 1 until db.relations.size) {
    val v1 = V(db.relations(relation1))
    val v2 = V(db.relations(relation2))
    fg.buildFactor(Seq(v1, v2))(_ map (_ => new VectorMsgs)) {
      e => new CellLogisticLoss2(e(0), e(1), 0.0)
    }
  }

  //create L2 regularizer for columns
  for (rel <- db.relations) {
    val v = V(rel)
    fg.buildFactor(Seq(v))(_ map (_ => new VectorMsgs)) {
      e => new L2Regularizer(e(0), 0.01)
    }
  }

  //create L2 regularizer for rows
  for (ent <- db.arg1s) {
    val a = A(ent)
    fg.buildFactor(Seq(a))(_ map (_ => new VectorMsgs)) {
      e => new L2Regularizer(e(0), 0.01)
    }
  }


  fg.build()
  //GradientBasedOptimizer(fg, new OnlineTrainer(_, new AdaGrad(), 100, 10))
  GradientBasedOptimizer(fg, new BatchTrainer(_, new AdaGrad(), 100))

  println("train:")
  println(db.toVerboseString(showTrain = true))
  println()

  println("predicted:")
  println(db.toVerboseString())
}

/**
 * @author Sebastian Riedel
 */
object ExtremeGeometricMF extends App {

  import ml.wolfe.Wolfe._

  val k             = 2
  val numRows       = 10
  val numCols       = 10
  val regW          = 0.1
  val regS          = 0.1
  val regBias       = 0.1
  val objNormalizer = 1.0 / (numCols * numCols)
  val subSample     = 1.0

  val db = new TensorKB(k)

  val rules = Map[(db.CellIx, db.CellIx), Double](
    //    ("r3","r4") -> 0,
    //    ("r4","r5"),
    //    ("r8", "r9") -> 0
    //("r5", "r6") -> 0
  )

  db.sampleTensor(numCols, numRows, 0, 0.2)
  //samples a matrix
  val fg   = db.toFactorGraph
  val data = db.cells
  //cols
  val V    = db.ix1ToNodeMap
  //rows
  val A    = db.ix2ToNodeMap

  val colIds = db.ix1ToNodeMap.keys.toArray.sortBy(_.toString)
  val rowIds = db.ix2ToNodeMap.keys.toArray.sortBy(_.toString)

  val pairCounts   = mutable.HashMap[(db.CellIx, db.CellIx), Int]() withDefaultValue 0
  val singleCounts = mutable.HashMap[db.CellIx, Int]() withDefaultValue 0


  val colScales = colIds.map(i => {
    i -> fg.addVectorNode(1)
  }).toMap

  val colBiases = colIds.map(i => {
    i -> fg.addVectorNode(1)
  }).toMap


  def count(col1: db.CellIx, col2: db.CellIx, b1: Boolean, b2: Boolean): Int = {
    (b1, b2) match {
      case (true, true) => pairCounts(col1, col2)
      case (true, false) => singleCounts(col1) - pairCounts(col1, col2)
      case (false, true) => singleCounts(col2) - pairCounts(col1, col2)
      case (false, false) => numRows - count(col1, col2, true, false) - count(col1, col2, false, true) - count(col1, col2, true, true)
    }
  }

  //  //change counts
  //  def addRule(body: db.CellIx, head: db.CellIx): Unit = {
  //    val oldCount11 = pairCounts(body, head)
  //    val oldCount1_ = singleCounts(body)
  //    val newCount11 = math.max(oldCount1_, oldCount11)
  //    val newCount_1 = singleCounts(head) + (newCount11 - oldCount11)
  //    pairCounts(body -> head) = newCount11
  //    singleCounts(head) = newCount_1
  //  }


  def freq(col1: db.CellIx, col2: db.CellIx, b1: Boolean, b2: Boolean) = {
    count(col1, col2, b1, b2) / numRows.toDouble
  }


  def ruleFreq(col1: db.CellIx, col2: db.CellIx, b1: Boolean, b2: Boolean) = {
    rules.get(col1 -> col2) match {
      case Some(add) =>
        val normalizer = numRows.toDouble + add
        (b1, b2) match {
          case (true, false) => 0.0
          case (true, true) => (add + count(col1, col2, true, false) + count(col1, col2, true, true)) / normalizer
          case (x, y) => count(col1, col2, x, y) / normalizer
        }
      case _ =>
        freq(col1, col2, b1, b2)
    }
  }


  //need to get counts from each row
  for (row <- rowIds) {
    val cells = db.getBy2(row).toArray
    for (cell <- cells) singleCounts(cell._1) += 1
    for (i <- 0 until cells.size; j <- i + 1 until cells.size) {
      pairCounts(cells(i)._1 -> cells(j)._1) += 1
    }
  }

  println(pairCounts.take(10).mkString("\n"))
  println(singleCounts.take(10).mkString("\n"))


  println("Building factors")
  //create pairwise factors
  for (col1Index <- 0 until colIds.length; col2Index <- col1Index + 1 until colIds.size) {
    val col1 = db.relations(col1Index)
    val col2 = db.relations(col2Index)

    val v1 = V(col1)
    val v2 = V(col2)

    val s1 = colScales(col1)
    val s2 = colScales(col2)

    val eta1 = colBiases(col1)
    val eta2 = colBiases(col2)

    //build 3 * 2 factors (1,1), (1,0), and (0,1)
    //First: (1,1)
    for (b1 <- Seq(true, false); b2 <- Seq(true, false)) {
      val freq_b1b2 = ruleFreq(col1, col2, b1, b2)
      val ignore = (!b1 || !b2) && (random.nextDouble() < (1.0 - subSample))
      if (freq_b1b2 > 0 && !ignore) {
        val scale = freq_b1b2 * objNormalizer // numCols// (numCols * numCols)
        //biases
        //learn the left cell
        fg.buildFactor(Seq(v1, v2, s1, eta1))(_ map (_ => new VectorMsgs)) {
          e => new LogPairwiseScaleBias(e(0), e(1), e(2), e(3), scale, I(b1), I(b1), I(b2))
        }
        //learn the right cell
        fg.buildFactor(Seq(v2, v1, s2, eta2))(_ map (_ => new VectorMsgs)) {
          e => new LogPairwiseScaleBias(e(0), e(1), e(2), e(3), scale, I(b2), I(b2), I(b1))
        }

        //        //learn the left cell
        //        fg.buildFactor(Seq(v1, v2, s1, s2))(_ map (_ => new VectorMsgs)) {
        //          e => new LogPairwiseWeighted(e(0), e(1), e(2), e(3), scale, I(b1), I(b1), I(b2))
        //        }
        //        //learn the right cell
        //        fg.buildFactor(Seq(v1, v2, s1, s2))(_ map (_ => new VectorMsgs)) {
        //          e => new LogPairwiseWeighted(e(1), e(0), e(3), e(2), scale, I(b2), I(b2), I(b1))
        //        }

      }
    }
  }

  //create L2 regularizers
  for (col <- colIds) {
    val v = V(col)
    fg.buildFactor(Seq(v))(_ map (_ => new VectorMsgs)) {
      e => new L2Regularizer(e(0), regW * objNormalizer)
    }

    val s = colScales(col)
    fg.buildFactor(Seq(s))(_ map (_ => new VectorMsgs)) {
      e => new L2RegularizerOffset(e(0), new DenseTensor1(Array(1.0)), regS * objNormalizer)
    }

    val eta = colBiases(col)
    fg.buildFactor(Seq(eta))(_ map (_ => new VectorMsgs)) {
      e => new L2Regularizer(e(0), regBias * objNormalizer)
    }
  }

  fg.build()
  println("Optimizing...")

  //  GradientBasedOptimizer(fg, new OnlineTrainer(_, new AdaGrad(), 100, 100000))
  GradientBasedOptimizer(fg, new BatchTrainer(_, new AdaGrad(), 1000))

  for (col <- colIds) {
    println(s"$col: ${ colScales(col).variable.asVector.b(0) } *  ${ V(col).variable.asVector.b } + ${ colBiases(col).variable.asVector.b(0) }")
  }

  println("Number of terms:" + fg.factors.size)

  def fillActualEmbeddings(): Unit = {
    for (row <- rowIds) {
      val result = new DenseTensor1(k + 1)
      val cells = db.getBy2(row).toArray
      for ((col, _) <- cells) {
        val v = V(col).variable.asVector.b
        for (i <- 0 until k) result(i) = result(i) + v(i)
        //result +=(v, colScales(col).variable.asVector.b(0))
      }
      result(k) = 1.0
      A(row).variable.asVector.b = result
    }
    for (col <- colIds) {
      val v = V(col).variable.asVector.b
      val s = colScales(col).variable.asVector.b(0)
      val eta = colBiases(col).variable.asVector.b(0)
      val result = new DenseTensor1(k + 1)
      for (i <- 0 until k) result(i) = s * v(i)
      result(k) = eta
      V(col).variable.asVector.b = result
    }

  }

  fillActualEmbeddings()
  println("train:")
  println(db.toVerboseString(showTrain = true))
  println()

  println("predicted:")
  println(db.toVerboseString())


}


/**
 * @author Sebastian Riedel
 */
class L2PairwiseRegularizer(col1Edge: Edge, col2Edge: Edge, scale: Double = 1.0) extends Potential {
  //nodes of edges may change hence the def and not val.
  def col1Var = col1Edge.n.variable.asVector
  def col2Var = col2Edge.n.variable.asVector
  val col1Msgs = col1Edge.msgs.asVector
  val col2Msgs = col2Edge.msgs.asVector

  override def valueForCurrentSetting(): Double = {
    val v1 = col1Var.setting
    val v2 = col2Var.setting
    val s = v1 dot v2
    scale * s
  }

  override def valueAndGradientForAllEdges(): Double = {
    col1Msgs.f2n = col2Msgs.n2f * scale
    col2Msgs.f2n = col1Msgs.n2f * scale
    val score = scale * (col2Msgs.n2f dot col1Msgs.n2f)
    score
  }
}

/**
 * @author Sebastian Riedel
 */
class L2Regularizer(col1Edge: Edge, scale: Double = 1.0) extends Potential {
  //nodes of edges may change hence the def and not val.
  def col1Var = col1Edge.n.variable.asVector
  val col1Msgs = col1Edge.msgs.asVector

  override def valueForCurrentSetting(): Double = {
    val v1 = col1Var.setting
    val s = v1 dot v1
    -scale * s
  }

  override def valueAndGradientForAllEdges(): Double = {
    col1Msgs.f2n = col1Msgs.n2f * (-scale)
    val score = -scale * (col1Msgs.n2f dot col1Msgs.n2f)
    score
  }
}

/**
 * @author Sebastian Riedel
 */
class L2RegularizerOffset(col1Edge: Edge, offset: DenseTensor1, scale: Double = 1.0) extends Potential {
  //nodes of edges may change hence the def and not val.
  def col1Var = col1Edge.n.variable.asVector
  val col1Msgs = col1Edge.msgs.asVector

  override def valueForCurrentSetting(): Double = {
    val v1 = col1Var.setting
    val s = (v1 - offset) dot (v1 - offset)
    -scale * s
  }

  override def valueAndGradientForAllEdges(): Double = {
    col1Msgs.f2n = (col1Msgs.n2f - offset) * (-scale)
    val score = -scale * ((col1Msgs.n2f - offset) dot (col1Msgs.n2f - offset))
    score
  }
}

/**
 * @author Sebastian Riedel
 */
class LogRepulsion(col1Edge: Edge, col2Edge: Edge, scale: Double = 1.0) extends Potential {
  //nodes of edges may change hence the def and not val.
  def col1Var = col1Edge.n.variable.asVector
  def col2Var = col2Edge.n.variable.asVector
  val col1Msgs = col1Edge.msgs.asVector
  val col2Msgs = col2Edge.msgs.asVector

  override def valueForCurrentSetting(): Double = {
    val v1 = col1Var.setting
    val v2 = col2Var.setting
    val s = v1 dot v2
    -scale * s
  }

  override def valueAndGradientForAllEdges(): Double = {
    col1Msgs.f2n = col2Msgs.n2f * (-scale)
    col2Msgs.f2n = col1Msgs.n2f * (-scale)
    val score = -scale * (col2Msgs.n2f dot col1Msgs.n2f)
    score
  }
}

/**
 * @author Sebastian Riedel
 */
class LogPairwiseTwoNorm(col1Edge: Edge, col2Edge: Edge,
                         scale: Double = 1.0, truth: Double, b1: Double, b2: Double, bias: Double = 0.0) extends Potential {

  //col1Edge is the column we want to predict
  //truth is the target truth value for the (pseudo) cell
  //assert(truth == b1)

  //nodes of edges may change hence the def and not val.
  def col1Var = col1Edge.n.variable.asVector
  def col2Var = col2Edge.n.variable.asVector
  val col1Msgs = col1Edge.msgs.asVector
  val col2Msgs = col2Edge.msgs.asVector

  override def valueForCurrentSetting(): Double = {
    val w1 = col1Var.setting
    val w2 = col2Var.setting
    val phi = b1 + b2 * (w1 dot w2) / w2.twoNormSquared + bias
    val logZ = math.log(1 + math.exp(phi))
    val logPi = truth * phi - logZ
    scale * logPi
  }

  override def valueAndGradientForAllEdges(): Double = {
    val w1 = col1Msgs.n2f
    val w2 = col2Msgs.n2f
    val w2NormSquared = w2.twoNormSquared
    val w1DotW2 = w1 dot w2
    val phi = b1 + b2 * w1DotW2 / w2NormSquared + bias
    val logZ = math.log(1 + math.exp(phi))
    val pi = math.exp(phi - logZ)

    val gradW1Vector = w2 / w2NormSquared
    val gradW2Vector = w1 * w2NormSquared - w2 * w1DotW2
    val gradW1 = gradW1Vector * (scale * (truth - pi))
    val gradW2 = gradW2Vector * (scale * (truth - pi) * b2)

    col1Msgs.f2n = gradW1
    col2Msgs.f2n = gradW2
    val logProb = truth * phi - logZ
    val prob = math.exp(logProb)
    if (gradW2.twoNorm > 1000) {
      val k = 0
      //println("Large gradient")
    }
    scale * logProb
  }
}

/**
 * @author Sebastian Riedel
 */
class LogPairwiseWeighted(val col1EdgeToBePredicted: Edge, val col2Edge: Edge,
                          val scale1Edge: Edge, val scale2Edge: Edge,
                          val scale: Double = 1.0, truth: Double, val b1: Double, val b2: Double) extends Potential {

  //col1Edge is the column we want to predict
  //truth is the target truth value for the (pseudo) cell
  assert(truth == b1)

  //nodes of edges may change hence the def and not val.
  def col1Var = col1EdgeToBePredicted.n.variable.asVector
  def col2Var = col2Edge.n.variable.asVector
  def scale1Var = scale1Edge.n.variable.asVector
  def scale2Var = scale2Edge.n.variable.asVector

  val col1Msgs   = col1EdgeToBePredicted.msgs.asVector
  val col2Msgs   = col2Edge.msgs.asVector
  val scale1Msgs = scale1Edge.msgs.asVector
  val scale2Msgs = scale2Edge.msgs.asVector

  val activeCount = b1 + b2
  val b1Norm      = if (activeCount == 0.0) 0.0 else b1 / activeCount
  val b2Norm      = if (activeCount == 0.0) 0.0 else b2 / activeCount

  override def valueAndGradientForAllEdges(): Double = {
    val w1 = col1Msgs.n2f
    val w2 = col2Msgs.n2f
    val s1 = scale1Msgs.n2f
    val s2 = scale2Msgs.n2f

    if (col1Msgs.f2n == null) col1Msgs.f2n = new DenseTensor1(w1.size)
    if (col2Msgs.f2n == null) col2Msgs.f2n = new DenseTensor1(w2.size)
    if (scale1Msgs.f2n == null) scale1Msgs.f2n = new DenseTensor1(1)
    if (scale2Msgs.f2n == null) scale2Msgs.f2n = new DenseTensor1(1)


    val w1NormSquared = w1.twoNormSquared
    val w1DotW2 = w1 dot w2
    val phi = b1Norm * s1(0) * w1NormSquared + b2Norm * s2(0) * w1DotW2
    val logZ = math.log(1 + math.exp(phi))
    val pi = math.exp(phi - logZ)
    val rate = scale * (truth - pi)

    col1Msgs.f2n := 0.0
    col1Msgs.f2n +=(w1, rate * s1(0) * b1Norm)
    col1Msgs.f2n +=(w2, rate * s2(0) * b2Norm)

    col2Msgs.f2n := 0.0
    col2Msgs.f2n +=(w1, rate * s2(0) * b2Norm)

    scale1Msgs.f2n(0) = rate * b1Norm * w1NormSquared
    scale2Msgs.f2n(0) = rate * b2Norm * w1DotW2

    val logProb = truth * phi - logZ
    if (scale2Msgs.f2n.twoNorm > 10) {
      val k = 1
    }
    val prob = math.exp(logProb)
    scale * logProb
  }
}


/**
 * @author Sebastian Riedel
 */
class LogPairwiseScaleBias(val col1EdgeToBePredicted: Edge, val col2Edge: Edge,
                           val scale1Edge: Edge, val bias1Edge: Edge,
                           val scale: Double = 1.0, val truth: Double,
                           val b1: Double, val b2: Double) extends Potential {

  //col1Edge is the column we want to predict
  //truth is the target truth value for the (pseudo) cell
  //assert(truth == b1)

  val col1Msgs   = col1EdgeToBePredicted.msgs.asVector
  val col2Msgs   = col2Edge.msgs.asVector
  val scale1Msgs = scale1Edge.msgs.asVector
  val bias1Msgs  = bias1Edge.msgs.asVector

  override def valueAndGradientForAllEdges(): Double = {
    val w1 = col1Msgs.n2f
    val w2 = col2Msgs.n2f
    val s1 = scale1Msgs.n2f
    val e1 = bias1Msgs.n2f

    if (col1Msgs.f2n == null) col1Msgs.f2n = new DenseTensor1(w1.size)
    if (col2Msgs.f2n == null) col2Msgs.f2n = new DenseTensor1(w2.size)
    if (scale1Msgs.f2n == null) scale1Msgs.f2n = new DenseTensor1(1)
    if (bias1Msgs.f2n == null) bias1Msgs.f2n = new DenseTensor1(1)


    val w1NormSquared = w1.twoNormSquared
    val w1DotW2 = w1 dot w2
    val phi = b1 * s1(0) * w1NormSquared + b2 * s1(0) * w1DotW2 + e1(0)
    val logZ = math.log(1 + math.exp(phi))
    val pi = math.exp(phi - logZ)
    val rate = scale * (truth - pi)

    col1Msgs.f2n := 0.0
    if (b1 != 0.0) col1Msgs.f2n +=(w1, rate * s1(0) * b1)
    if (b2 != 0.0) col1Msgs.f2n +=(w2, rate * s1(0) * b2)

    col2Msgs.f2n := 0.0
    if (b2 != 0.0) col2Msgs.f2n +=(w1, rate * s1(0) * b2)

    scale1Msgs.f2n(0) = rate * (b1 * w1NormSquared + b2 * w1DotW2)
    bias1Msgs.f2n(0) = rate

    val logProb = truth * phi - logZ
    //    if (bias1Msgs.f2n.twoNorm > 10) {
    //      val k = 1
    //    }
    val prob = math.exp(logProb)
    scale * logProb
  }
}

class SingleColumnBias(val biasEdge: Edge, scale: Double, truth: Double) extends Potential {

  import math._

  val biasMsg = biasEdge.msgs.asVector
  override def valueAndGradientForAllEdges() = {
    if (biasMsg.f2n == null) biasMsg.f2n = new DenseTensor1(1)

    val b = biasMsg.n2f(0)
    val logZ = log(1 + exp(b))
    val pi = exp(b - logZ)
    val obj = truth * log(pi) + (1.0 - truth) * log(1.0 - pi)
    biasMsg.f2n(0) = scale * (truth * (1.0 - pi) + (1.0 - truth) * (0.0 - pi))
    scale * obj
  }
}


/**
 * @author Sebastian Riedel
 */
class LogPairwiseWeightedScaleBias(val col1EdgeToBePredicted: Edge, val col2Edge: Edge,
                                   val scale1Edge: Edge, val bias1Edge: Edge,
                                   val mult1Edge: Edge, val mult2Edge: Edge,
                                   val scale: Double = 1.0, val truth: Double,
                                   val b1: Double, val b2: Double) extends Potential {

  //col1Edge is the column we want to predict
  //truth is the target truth value for the (pseudo) cell
  //assert(truth == b1)

  val col1Msgs   = col1EdgeToBePredicted.msgs.asVector
  val col2Msgs   = col2Edge.msgs.asVector
  val scale1Msgs = scale1Edge.msgs.asVector
  val bias1Msgs  = bias1Edge.msgs.asVector
  val mult1Msgs  = mult1Edge.msgs.asVector
  val mult2Msgs  = mult2Edge.msgs.asVector

  override def valueAndGradientForAllEdges(): Double = {
    val w1 = col1Msgs.n2f
    val w2 = col2Msgs.n2f
    val s1 = scale1Msgs.n2f(0)
    val e1 = bias1Msgs.n2f(0)
    val m1 = mult1Msgs.n2f(0)
    val m2 = mult2Msgs.n2f(0)

    if (col1Msgs.f2n == null) col1Msgs.f2n = new DenseTensor1(w1.size)
    if (col2Msgs.f2n == null) col2Msgs.f2n = new DenseTensor1(w2.size)
    if (scale1Msgs.f2n == null) scale1Msgs.f2n = new DenseTensor1(1)
    if (bias1Msgs.f2n == null) bias1Msgs.f2n = new DenseTensor1(1)
    if (mult1Msgs.f2n == null) mult1Msgs.f2n = new DenseTensor1(1)
    if (mult2Msgs.f2n == null) mult2Msgs.f2n = new DenseTensor1(1)


    val w1NormSquared = w1.twoNormSquared
    val w1DotW2 = w1 dot w2
    val phi = b1 * m1 * s1 * w1NormSquared + b2 * m2 * s1 * w1DotW2 + e1
    val logZ = math.log(1 + math.exp(phi))
    val pi = math.exp(phi - logZ)
    val rate = scale * (truth - pi)

    col1Msgs.f2n := 0.0
    if (b1 != 0.0) col1Msgs.f2n +=(w1, rate * s1 * b1 * m1)
    if (b2 != 0.0) col1Msgs.f2n +=(w2, rate * s1 * b2 * m1)

    col2Msgs.f2n := 0.0
    if (b2 != 0.0) col2Msgs.f2n +=(w1, rate * s1 * b2 * m2)

    if (b1 != 0.0) mult1Msgs.f2n(0) = rate * b1 * s1 * w1NormSquared
    if (b2 != 0.0) mult2Msgs.f2n(0) = rate * b2 * s1 * w1DotW2

    scale1Msgs.f2n(0) = rate * (b1 * m1 * w1NormSquared + b2 * m2 * w1DotW2)
    bias1Msgs.f2n(0) = rate

    val logProb = truth * phi - logZ
    //    if (bias1Msgs.f2n.twoNorm > 10) {
    //      val k = 1
    //    }
    val prob = math.exp(logProb)
    scale * logProb
  }
}

class JointPotential(weights1: Edge, bias1: Edge, scale1: Edge, mult1: Edge,
                     weights2: Edge, bias2: Edge, scale2: Edge, mult2: Edge,
                     trueProbOf1Given2: Double, trueProbOf2Given1: Double, marg1: Double, marg2: Double,
                     regWeights: Double, regBias: Double, regScale: Double, regMult: Double,
                     priorBias: Double, priorScale: Double, priorMult: Double,
                     arg1Coeff: Double, arg2Coeff: Double,
                     termWeight1: Double = 1.0, termWeight2: Double = 1.0) extends Potential {

  def nn[T1, T2](t: T1)(f: T1 => T2): T2 = if (t != null) f(t) else null.asInstanceOf[T2]

  val weights1Msgs = weights1.msgs.asVector
  val bias1Msgs    = nn(bias1)(_.msgs.asVector)
  val scale1Msgs   = nn(scale1)(_.msgs.asVector)
  val mult1Msgs    = nn(mult1)(_.msgs.asVector)

  val weights2Msgs = weights2.msgs.asVector
  val bias2Msgs    = nn(bias2)(_.msgs.asVector)
  val scale2Msgs   = nn(scale2)(_.msgs.asVector)
  val mult2Msgs    = nn(mult2)(_.msgs.asVector)


  def initMsgs(k: Int): Unit = {
    if (weights1Msgs.f2n == null) weights1Msgs.f2n = new DenseTensor1(k)
    if (weights2Msgs.f2n == null) weights2Msgs.f2n = new DenseTensor1(k)
    if (scale1Msgs != null && scale1Msgs.f2n == null) scale1Msgs.f2n = new DenseTensor1(1)
    if (scale2Msgs != null && scale2Msgs.f2n == null) scale2Msgs.f2n = new DenseTensor1(1)
    if (bias1Msgs != null && bias1Msgs.f2n == null) bias1Msgs.f2n = new DenseTensor1(1)
    if (bias2Msgs != null && bias2Msgs.f2n == null) bias2Msgs.f2n = new DenseTensor1(1)
    if (mult1Msgs != null && mult1Msgs.f2n == null) mult1Msgs.f2n = new DenseTensor1(1)
    if (mult2Msgs != null && mult2Msgs.f2n == null) mult2Msgs.f2n = new DenseTensor1(1)

    weights1Msgs.f2n := 0.0
    weights2Msgs.f2n := 0.0
    if (scale1Msgs != null) scale1Msgs.f2n := 0.0
    if (scale2Msgs != null) scale2Msgs.f2n := 0.0
    if (bias1Msgs != null) bias1Msgs.f2n := 0.0
    if (bias2Msgs != null) bias2Msgs.f2n := 0.0
    if (mult1Msgs != null) mult1Msgs.f2n := 0.0
    if (mult2Msgs != null) mult2Msgs.f2n := 0.0
  }

  def logConditional(targetWeights: VectorMsgs, targetBias: VectorMsgs, targetScale: VectorMsgs,
                     obsWeights: VectorMsgs, obsMult: VectorMsgs, trueProb: Double, weight: Double = 1.0) = {
    val tw = targetWeights.n2f
    val tb = if (targetBias != null) targetBias.n2f(0) else priorBias
    val ts = if (targetScale != null) targetScale.n2f(0) else priorScale

    val ow = obsWeights.n2f
    val om = if (obsMult != null) obsMult.n2f(0) else priorMult

    val twDotOw = tw dot ow
    val phi = twDotOw * om * ts + tb
    val logZ = log(1 + exp(phi))
    val probTrue = math.exp(phi - logZ)
    val obj = trueProb * log(probTrue) + ((1.0 - trueProb) * log(1.0 - probTrue))
    val trueRate = (1.0 - probTrue) * trueProb
    val falseRate = (0.0 - probTrue) * (1.0 - trueProb)
    val rate = (trueRate + falseRate) * weight

    targetWeights.f2n +=(ow, om * ts * rate)
    if (targetScale != null) targetScale.f2n(0) += twDotOw * om * rate
    if (targetBias != null) targetBias.f2n(0) += rate

    obsWeights.f2n +=(tw, om * ts * rate)
    if (obsMult != null) obsMult.f2n(0) += twDotOw * ts * rate
    if (obj.isNaN) {
      println("NaN")
    }
    obj
  }

  def marginal(targetBias: VectorMsgs, trueProb: Double, coeff: Double) = {
    val b = if (targetBias != null) targetBias.n2f(0) else priorBias
    val logZ = log(1 + exp(b))
    val probTrue = math.exp(b - logZ)
    val obj = (trueProb * log(probTrue) + (1.0 - trueProb) * log(1.0 - probTrue)) * coeff
    val trueRate = (1.0 - probTrue) * trueProb * coeff
    val falseRate = (0.0 - probTrue) * (1.0 - trueProb) * coeff
    if (targetBias != null) targetBias.f2n(0) += trueRate + falseRate
    obj
  }

  def regularizers() = {
    def sq(num: Double) = num * num
    weights1Msgs.f2n +=(weights1Msgs.n2f, -regWeights * 2.0 * arg1Coeff)
    weights2Msgs.f2n +=(weights2Msgs.n2f, -regWeights * 2.0 * arg2Coeff)
    if (bias1Msgs != null) bias1Msgs.f2n(0) += -regBias * (bias1Msgs.n2f(0) - priorBias) * 2.0 * arg1Coeff
    if (bias2Msgs != null) bias2Msgs.f2n(0) += -regBias * (bias2Msgs.n2f(0) - priorBias) * 2.0 * arg2Coeff
    if (scale1Msgs != null) scale1Msgs.f2n(0) += -regScale * (scale1Msgs.n2f(0) - priorScale) * 2.0 * arg1Coeff
    if (scale2Msgs != null) scale2Msgs.f2n(0) += -regScale * (scale2Msgs.n2f(0) - priorScale) * 2.0 * arg2Coeff

    if (mult1Msgs != null) mult1Msgs.f2n(0) += -regMult * (mult1Msgs.n2f(0) - priorMult) * 2.0 * arg1Coeff
    if (mult2Msgs != null) mult2Msgs.f2n(0) += -regMult * (mult2Msgs.n2f(0) - priorMult) * 2.0 * arg2Coeff
    var result = 0.0
    result += -regWeights * (weights1Msgs.n2f.twoNormSquared * arg1Coeff + weights2Msgs.n2f.twoNormSquared * arg2Coeff)
    //todo: use priors if null
    if (bias1Msgs != null)
      result += -regBias * (sq(bias1Msgs.n2f(0) - priorBias) * arg1Coeff + sq(bias2Msgs.n2f(0) - priorBias) * arg2Coeff)
    if (mult1Msgs != null)
      result += -regMult * (sq(mult1Msgs.n2f(0) - priorMult) * arg1Coeff + sq(mult2Msgs.n2f(0) - priorMult) * arg2Coeff)
    if (scale1Msgs != null)
      result += -regScale * (sq(scale1Msgs.n2f(0) - priorScale) * arg1Coeff + sq(scale2Msgs.n2f(0) - priorScale) * arg2Coeff)
    result
  }

  override def valueAndGradientForAllEdges() = {
    val k = weights1Msgs.n2f.size
    initMsgs(k)
    var result = 0.0
    result += logConditional(weights1Msgs, bias1Msgs, scale1Msgs, weights2Msgs, mult2Msgs, trueProbOf1Given2, termWeight2)
    result += logConditional(weights2Msgs, bias2Msgs, scale2Msgs, weights1Msgs, mult1Msgs, trueProbOf2Given1, termWeight1)
    result += marginal(bias1Msgs, marg1, arg1Coeff)
    result += marginal(bias2Msgs, marg2, arg2Coeff)
    result += regularizers()
    result
  }
}

class L2DistanceBasedPotential(w1Edge: Edge, w2Edge: Edge, bias1Edge: Edge, bias2Edge: Edge,
                               prob1given2: Double, prob2given1: Double, scale: Double,
                               regWeights: Double,
                               regBias: Double,
                               priorBias: Double,
                               arg1Coeff: Double, arg2Coeff: Double) extends Potential {

  val weights1Msgs = w1Edge.msgs.asVector
  val weights2Msgs = w2Edge.msgs.asVector

  val bias1Msgs = bias1Edge.msgs.asVector
  val bias2Msgs = bias2Edge.msgs.asVector

  override def valueAndGradientForAllEdges() = {
    initMsgs(weights1Msgs.n2f.size)
    var result = 0.0
    result += conditionalGradient(weights1Msgs, weights2Msgs, bias1Msgs, prob1given2)
    result += conditionalGradient(weights2Msgs, weights1Msgs, bias2Msgs, prob2given1)
    result += regularizers()
    result
  }

  def initMsgs(k: Int): Unit = {
    if (weights1Msgs.f2n == null) weights1Msgs.f2n = new DenseTensor1(k)
    if (weights2Msgs.f2n == null) weights2Msgs.f2n = new DenseTensor1(k)
    if (bias1Msgs != null && bias1Msgs.f2n == null) bias1Msgs.f2n = new DenseTensor1(1)
    if (bias2Msgs != null && bias2Msgs.f2n == null) bias2Msgs.f2n = new DenseTensor1(1)
    weights1Msgs.f2n := 0.0
    weights2Msgs.f2n := 0.0
    if (bias1Msgs != null) bias1Msgs.f2n := 0.0
    if (bias2Msgs != null) bias2Msgs.f2n := 0.0
  }


  def conditionalGradient(target: VectorMsgs, obs: VectorMsgs, targetBias: VectorMsgs, targetProb: Double) = {
    val t = target.n2f
    val o = obs.n2f
    val eta = targetBias.n2f(0)
    val dist = t.l2Similarity(o)
    val dist2 = dist * dist
    val phi = eta - dist2
    val pi = exp(phi - log1p(exp(phi)))
    val rate = (targetProb * (1.0 - pi) + (1 - targetProb) * (0.0 - pi)) * scale
    val obj = scale * (targetProb * log(pi) + (1 - targetProb) * log(1.0 - pi))
    target.f2n +=(o, 2 * rate)
    target.f2n +=(t, -2 * rate)
    obs.f2n +=(t, 2 * rate)
    obs.f2n +=(o, -2 * rate)
    targetBias.f2n(0) += rate
    obj
  }

  def regularizers() = {
    def sq(num: Double) = num * num
    weights1Msgs.f2n +=(weights1Msgs.n2f, -regWeights * 2.0 * arg1Coeff * scale)
    weights2Msgs.f2n +=(weights2Msgs.n2f, -regWeights * 2.0 * arg2Coeff * scale)
    if (bias1Msgs != null) bias1Msgs.f2n(0) += -regBias * (bias1Msgs.n2f(0) - priorBias) * 2.0 * arg1Coeff * scale
    if (bias2Msgs != null) bias2Msgs.f2n(0) += -regBias * (bias2Msgs.n2f(0) - priorBias) * 2.0 * arg2Coeff * scale
    var result = 0.0
    result += -regWeights * scale * (weights1Msgs.n2f.twoNormSquared * arg1Coeff + weights2Msgs.n2f.twoNormSquared * arg2Coeff)
    if (bias1Msgs != null)
      result += -regBias * scale * (sq(bias1Msgs.n2f(0) - priorBias) * arg1Coeff + sq(bias2Msgs.n2f(0) - priorBias) * arg2Coeff)
    result
  }

}


