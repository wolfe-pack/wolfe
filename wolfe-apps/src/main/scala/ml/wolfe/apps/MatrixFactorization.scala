package ml.wolfe.apps

import breeze.optimize.StochasticGradientDescent
import cc.factorie.model.{WeightsMap, WeightsSet}
import cc.factorie.optimize.{AdaGrad, OnlineTrainer}
import ml.wolfe.FactorGraph.Node
import ml.wolfe.{GradientBasedOptimizer, FactorGraph}
import ml.wolfe.fg.{L2Regularization, VectorMsgs, CellLogisticLoss}

import scala.annotation.tailrec
import scala.util.Random

/**
 * @author Sebastian Riedel
 */
object MatrixFactorization extends App {

  val k      = 10
  val random = new Random(0l)

  val fg = new FactorGraph

  val numRows = 10
  val numCols = 10
  val cellDensity = 0.1
  val numObservedCells = (numRows * numCols * cellDensity).toInt

  val rows = (0 until numRows).map(i => "e" + i).toArray
  val cols = (0 until numCols).map(i => "r" + i).toArray

  val data = (0 until numObservedCells).map(i => {
    val row = random.nextInt(numRows)
    val col = random.nextInt(numCols)
    rows(row) -> cols(col)
  }).toSet

  val A = (rows map (p => p -> fg.addVectorNode(k))).toMap
  val V = (cols map (r => r -> fg.addVectorNode(k))).toMap

  for (d <- data) {
    val a = A(d._1)
    val v = V(d._2)

    //create positive fact factor
    fg.buildFactor(Seq(a, v))(
      _ map (_ => new VectorMsgs)) { e => new CellLogisticLoss(e(0), e(1), 1.0, 0.01) with L2Regularization }

    //also create a sampled stochastic negative factor in the same column
    //fixme: not resampled???
    fg.buildStochasticFactor(Seq(v, sampleRow(d._1)))(
      _ map (_ => new VectorMsgs)) { e => new CellLogisticLoss(e(0), e(1), 0.0, 0.01) with L2Regularization }
  }

  @tailrec
  def sampleRow(col: String, attempts: Int = 1000): Node =
    if (attempts == 0) A(rows(random.nextInt(numRows)))
    else {
      val row = rows(random.nextInt(numRows))
      if (data.contains(row -> col)) sampleRow(col, attempts - 1)
      else A(row)
    }

  fg.build()

  GradientBasedOptimizer(fg, new OnlineTrainer(_, new AdaGrad(), 100,10))



  def sig(x: Double) = 1.0 / (1.0 + math.exp(-x))

  implicit class ColorString(string: String) {
    import Console._
    def red() = colorize(RED)
    def blue() = colorize(BLUE)
    def cyan() = colorize(CYAN)
    def green() = colorize(GREEN)
    def yellow() = colorize(YELLOW)
    def white() = colorize(WHITE)
    def magenta() = colorize(MAGENTA)
    def bold() = BOLD + string + RESET
    def underlined() = UNDERLINED + string + RESET
    private def colorize(color: String) = color + string + RESET
  }

  println("train:")
  println("\t" + cols.mkString(" "*4))
  println(rows.map(r => r + "\t" + cols.map(c =>
    if (data.contains((r, c))) " 1  ".green() else " "*4
  ).mkString("  ")).mkString("\n"))

  println("predicted:")
  println("\t" + cols.mkString(" "*4))
  println(rows.map(r => r + "\t" + cols.map(c => {
    val p = sig(A(r).variable.asVector.b dot V(c).variable.asVector.b)
    val pString = "%4.2f".format(p)
    if (data.contains((r, c)))
      if (p >= 0.9) pString.green()
      else if (p >= 0.5) pString.yellow()
      else pString.red()
    else if (p >= 0.9) pString.magenta()
    else if (p >= 0.5) pString.cyan()
    else pString
  }).mkString("  ")).mkString("\n"))
}
