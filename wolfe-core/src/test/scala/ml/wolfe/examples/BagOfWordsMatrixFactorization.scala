package ml.wolfe.examples

import ml.wolfe._
import ml.wolfe.term.TermImplicits._
import ml.wolfe.term._
import util.Math._



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

  @domain case class Theta(rows: IndexedSeq[Vect], words: IndexedSeq[Vect])

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
  val adaParams = AdaGradParameters(epochs = 100, learningRate = 1, initParams = init)

  //do the training (argmax is a term, so it needs to be evaluated to do the optimization)
  val thetaStar = argmax(Thetas)(t => loss(t).argmaxBy(Argmaxer.adaGrad(adaParams))).eval()


}
