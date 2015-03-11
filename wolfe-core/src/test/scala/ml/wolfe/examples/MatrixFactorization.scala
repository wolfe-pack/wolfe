package ml.wolfe.examples

import ml.wolfe._
import ml.wolfe.term.TermImplicits._
import ml.wolfe.term._
import util.Math._

/**
 * This is a simple matrix factorization model with logistic loss, and cell-depended negative sampling.
 * @author riedel
 */
object MatrixFactorization extends App {

  //number of latent dimensions
  val k = 5

  //matrix size
  val rows = 10
  val cols = 10

  // the class of parameters
  @domain case class Theta(v: IndexedSeq[FactorieVector], a: IndexedSeq[FactorieVector])

  // the class of cells
  @domain case class Cell(row: Int, col: Int)

  // the set of possible parameters
  implicit val Thetas = Theta.Values(Seqs(Vectors(k), rows), Seqs(Vectors(k), cols))

  // the set of possible cells
  implicit val Cells = Cell.Values(Ints(0 until rows), Ints(0 until cols))

  // positive training data, as a term
  val trainingData = Seq(Cell(0, 0), Cell(1, 1)).toConst

  //call some user code to return a negative cell, uses term monad (so pos is an actual Cell, not a term)
  //note that this requires "cells" to be implicit, as it creates a new cells.Term
  def sampleNegCell(posTerm: Cells.Term) = for (pos <- posTerm) yield pos.copy(row = random.nextInt(rows))

  //the per cell loss
  def score(theta: Thetas.Term)(cell: Cells.Term) =
    theta.v(cell.row) dot theta.a(cell.col)

  //training loss, stochastic term based on sampling a positive cell, and then a negative based on it.
  def loss(t: Thetas.Term) = {
    //we sample a positive cell, and memoize the result
    val pos = mem(trainingData.sampleSequential)
    //based on the memoized positive cell, we sample a negative cell which needs to memoized because it will reappear several times
    val neg = mem(sampleNegCell(pos))
    //the loss based on positve and negative cell
    log(sigm(score(t)(pos))) + log(sigm(-score(t)(neg)))
  }

  //learning parameters
  val init = Settings(Thetas.createRandomSetting(random.nextGaussian() * 0.1))
  val adaParams = AdaGradParameters(iterations = 100, learningRate = 1, initParams = init)

  //do the training (argmax is a term, so it needs to be evaluated to do the optimization)
  val thetaStar = argmax2(Thetas)(t => loss(t).argmaxBy(Argmaxer.adaGrad2(adaParams))).eval2()


}

object BagOfWordsMatrixFactorization extends App {
  val k = 3

  //matrix size
  val numRows = 10
  val numCols = 10

  //number of distinct "words"
  val numWords = 100

  //how many words can one column have
  val maxWordsPerCol = 5

  @domain case class Col(words: IndexedSeq[Int])

  @domain case class Theta(rows: IndexedSeq[FactorieVector], words: IndexedSeq[FactorieVector])

  @domain case class Cell(row: Int, col: Int)

  implicit val Thetas = Theta.Values(
    Seqs(Vectors(k), numRows),
    Seqs(Vectors(k), numWords))

  implicit val Words = Ints(0 until numWords)
  implicit val Cols = Col.Values(Seqs(Words, 0, maxWordsPerCol))
  implicit val Cells = Cell.Values(Ints(0 until numRows), Ints(0 until numCols))

  val cols = Seq(Col(IndexedSeq(0, 1)), Col(IndexedSeq(1, 2))).toConst
  val trainingData = Seq(Cell(0, 0), Cell(1, 1)).toConst

  def sampleNegCell(posTerm: Cells.Term) = for (pos <- posTerm) yield pos.copy(row = random.nextInt(numRows))

  def score(t: Thetas.Term)(cell: Cells.Term) = {
    sum(cols(cell.col).words) { w => t.words(w) dot t.rows(cell.row)}
  }

  def loss(t: Thetas.Term) = {
    val pos = mem(trainingData.sampleSequential)
    val neg = mem(sampleNegCell(pos))
    log(sigm(score(t)(pos))) + log(sigm(-score(t)(neg)))
  }

  val init = Settings(Thetas.createRandomSetting(random.nextGaussian() * 0.1))
  val adaParams = AdaGradParameters(iterations = 100, learningRate = 1, initParams = init)

  //do the training (argmax is a term, so it needs to be evaluated to do the optimization)
  val thetaStar = argmax2(Thetas)(t => loss(t).argmaxBy(Argmaxer.adaGrad2(adaParams))).eval2()


}
