package ml.wolfe.model

import com.typesafe.scalalogging.slf4j.LazyLogging
import ml.wolfe._
import ml.wolfe.term.TermImplicits._
import ml.wolfe.term._
import ml.wolfe.util.Math._

/**
 * A simple matrix factorization model with a Log-Likelihood objective
 * @author rockt
 */
trait MatrixFactorization {
  //dimensions of the matrix
  def numRows: Int

  def numCols: Int

  //fixed rank for factorization
  def k: Int

  //the class of cells
  @domain case class Cell(row: Int, col: Int)

  //the set of possible cells
  implicit val Cells = Cell.Values(Ints(0 until numRows), Ints(0 until numCols))

  // the class of parameters
  @domain case class Theta(rows: IndexedSeq[Vect], cols: IndexedSeq[Vect])

  //the set of possible parameters
  implicit val Thetas = Theta.Values(Seqs(Vectors(k), numRows), Seqs(Vectors(k), numCols))

  //number of AdaGrad epochs over the training data
  def epochs: Int

  //initial learning rate
  def alpha: Double

  def delta: Double = 0.1

  //l2 regularization parameter
  def lambda: DoubleTerm = -0.01

  //a sequence of positive training cells
  def trainingData: Seq[Cell]

  lazy val trainingDataTerm = trainingData.toConst

  //a user-defined function that samples a negative cell based on a positive one
  def sampleNegCell(pos: Cell): Cell

  private def sampleNegCellTerm(pos: Cells.Term): Cells.Term = pos convertValue(p => sampleNegCell(p))

  //where learned parameters will be stored
  var thetaStar: Thetas.Value = _

  //the per cell score
  def score(theta: Thetas.Term)(cell: Cells.Term): DoubleTerm = theta.rows(cell.row) dot theta.cols(cell.col)

  def regularize(theta: Thetas.Term)(pos: Cells.Term, neg: Cells.Term): DoubleTerm =
    sum(Seq(theta.cols(pos.col), theta.rows(pos.row), theta.rows(neg.row))) { v => v.l2() * lambda }

  implicit val rand = random

  //training loss, stochastic term based on sampling a positive cell, and then a negative based on it.
  def loss(t: Thetas.Term): DoubleTerm = {
    //we sample a positive cell, and memoize the result
    shuffled(trainingDataTerm) { pos =>
      //based on the memoized positive cell, we sample a negative cell which needs to be memoized because it will reappear several times
      val neg = sampleNegCellTerm(pos)
      //the loss based on positive and negative cell
      log(sigm(score(t)(pos))) + log(sigm(-score(t)(neg))) + regularize(t)(pos, neg)
    }
  }

  def train() = {
    //learning parameters
    val init = Settings(Thetas.createRandomSetting(random.nextGaussian() * 0.1))
    val adaParams = AdaGradParameters(epochs = epochs, learningRate = alpha, initParams = init, delta = delta)

    //do the training (argmax is a term, so it needs to be evaluated to do the optimization)
    thetaStar = argmax(Thetas)(t => loss(t).argmaxBy(Argmaxer.adaGrad(adaParams))).eval()
  }

  lazy val probFun = fun(Cells) { x => sigm(score(thetaStar.toConst)(x)) }

  def predict(row: Int, col: Int) = probFun(Cell(row, col))
}

object MatrixFactorization extends LazyLogging {
  def train(data: Seq[(Int, Int)], kParam: Int = 5, alphaParam: Double = 0.1, epochsParam: Int = 100, lambdaParam: Double = 0.0): MatrixFactorization = {
    val mf = new MatrixFactorization {
      def k: Int = kParam

      def epochs: Int = epochsParam

      def alpha: Double = alphaParam

      override def lambda: DoubleTerm = -lambdaParam

      lazy val trainingData: Seq[Cell] = data.map(t => Cell(t._1, t._2))
      lazy val trainingSet = scala.collection.immutable.HashSet(trainingData:_*)

      lazy val numRows: Int = data.maxBy(_._1)._1 + 1

      lazy val numCols: Int = data.maxBy(_._2)._2 + 1

      //warning: this is slow!
      def sampleNegCell(pos: Cell): Cell = {
        def inner(pos: Cell, attempts: Int): Cell = {
          val sample = Cell(rand.nextInt(numRows), pos.col)
          if (attempts == 0) {
            logger.warn("Couldn't sample a negative cell for " + pos)
            sample
          } else if (trainingSet.contains(sample)) {
            inner(pos, attempts - 1)
          } else sample
        }
        inner(pos, 100)
      }
    }
    mf.train()
    mf
  }
}

