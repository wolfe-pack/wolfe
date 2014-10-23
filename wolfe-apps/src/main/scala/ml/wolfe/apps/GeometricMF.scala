package ml.wolfe.apps

import cc.factorie.la.DenseTensor1
import cc.factorie.optimize.{BatchTrainer, AdaGrad, OnlineTrainer}
import ml.wolfe.FactorGraph.Edge
import ml.wolfe.{Wolfe, GradientBasedOptimizer}
import ml.wolfe.fg._

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

  import Wolfe._

  val k  = 3
  val db = new TensorKB(k)
  db.sampleTensor(10, 10, 0, 0.1)
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

  val numRows = rowIds.size
  val numCols = colIds.size

  val scalingWeights = colIds.map(i => {
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

  //change counts
  def addRule(body: db.CellIx, head: db.CellIx): Unit = {
    val oldCount11 = pairCounts(body, head)
    val oldCount1_ = singleCounts(body)
    val newCount11 = math.max(oldCount1_, oldCount11)
    val newCount_1 = singleCounts(head) + (newCount11 - oldCount11)
    pairCounts(body -> head) = newCount11
    singleCounts(head) = newCount_1
  }

  def freq(col1: db.CellIx, col2: db.CellIx, b1: Boolean, b2: Boolean) = {
    count(col1, col2, b1, b2) / numRows.toDouble
  }


  //need to get counts from each row
  for (row <- rowIds) {
    val cells = db.getBy2(row).toArray
    for (cell <- cells) singleCounts(cell._1) += 1
    for (i <- 0 until cells.size; j <- i + 1 until cells.size) {
      pairCounts(cells(i)._1 -> cells(j)._1) += 1
    }
  }

  println(pairCounts.mkString("\n"))
  println(singleCounts.mkString("\n"))

  addRule("r9", "r10")

  //create pairwise factors
  for (col1Index <- 0 until colIds.length; col2Index <- col1Index + 1 until colIds.size) {
    val col1 = db.relations(col1Index)
    val col2 = db.relations(col2Index)

    val v1 = V(col1)
    val v2 = V(col2)

    val s1 = scalingWeights(col1)
    val s2 = scalingWeights(col2)

    //build 3 * 2 factors (1,1), (1,0), and (0,1)
    //First: (1,1)
    for (b1 <- Seq(true, false); b2 <- Seq(true, false)) {
      val count_b1b2 = freq(col1, col2, b1, b2)
      if (count_b1b2 > 0) {
        //learn the left cell
        fg.buildFactor(Seq(v1, v2, s1, s2))(_ map (_ => new VectorMsgs)) {
          e => new LogPairwiseWeighted(e(0), e(1), e(2), e(3), count_b1b2, I(b1), I(b1), I(b2))
        }
        //learn the right cell
        fg.buildFactor(Seq(v1, v2, s1, s2))(_ map (_ => new VectorMsgs)) {
          e => new LogPairwiseWeighted(e(1), e(0), e(3), e(2), count_b1b2, I(b2), I(b2), I(b1))
        }
      }
    }
  }

  //create L2 regularizer for columns
  for (col <- colIds) {
    val v = V(col)
    fg.buildFactor(Seq(v))(_ map (_ => new VectorMsgs)) {
      e => new L2Regularizer(e(0), 0.1)
    }
  }

  //create L2 regularizer for scaling weights
  for (col <- colIds) {
    val s = scalingWeights(col)
    fg.buildFactor(Seq(s))(_ map (_ => new VectorMsgs)) {
      e => new L2RegularizerOffset(e(0), new DenseTensor1(Array(1.0)), 0.1)
    }
  }

  fg.build()
  //GradientBasedOptimizer(fg, new OnlineTrainer(_, new AdaGrad(), 100, 10))
  GradientBasedOptimizer(fg, new BatchTrainer(_, new AdaGrad(), 1000))

  for (col <- colIds) {
    println(s"$col: ${ scalingWeights(col).variable.asVector.b(0) } ${ V(col).variable.asVector.b }")
  }

  def fillRowEmbeddings(): Unit = {
    for (row <- rowIds) {
      val result = new DenseTensor1(k)
      val cells = db.getBy2(row).toArray
      for ((col, _) <- cells) {
        val v = V(col).variable.asVector.b
        result +=(v, scalingWeights(col).variable.asVector.b(0))
      }
      A(row).variable.asVector.b = result
    }
  }


  fillRowEmbeddings()

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
class LogPairwiseWeighted(col1EdgeToBePredicted: Edge, col2Edge: Edge,
                          scale1Edge: Edge, scale2Edge: Edge,
                          scale: Double = 1.0, truth: Double, b1: Double, b2: Double) extends Potential {

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
    val phi = b1 * s1(0) * w1NormSquared + b2 * s2(0) * w1DotW2
    val logZ = math.log(1 + math.exp(phi))
    val pi = math.exp(phi - logZ)
    val rate = scale * (truth - pi)

    col1Msgs.f2n := 0.0
    col1Msgs.f2n +=(w1, rate * s1(0) * b1)
    col1Msgs.f2n +=(w2, rate * s2(0) * b2)

    col2Msgs.f2n := 0.0
    col2Msgs.f2n +=(w1, rate * s2(0) * b2)

    scale1Msgs.f2n(0) = rate * b1 * w1NormSquared
    scale2Msgs.f2n(0) = rate * b2 * w1DotW2

    val logProb = truth * phi - logZ
    if (scale2Msgs.f2n.twoNorm > 10) {
      val k = 1
    }
    val prob = math.exp(logProb)
    scale * logProb
  }
}



