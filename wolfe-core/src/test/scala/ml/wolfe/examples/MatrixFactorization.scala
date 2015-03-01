package ml.wolfe.examples

import ml.wolfe._
import ml.wolfe.term.TermImplicits._
import ml.wolfe.term.{Term, VarSeqDom, TypedTerm, domain}

/**
 * @author riedel
 */
object MatrixFactorization extends App {

  //number of latent dimensions
  val k = 3

  //matrix size
  val m = 3
  val n = 3

  // the class of parameters
  @domain case class Theta(v:IndexedSeq[FactorieVector],a:IndexedSeq[FactorieVector])

  // the class of cells
  @domain case class Cell(i:Int, j:Int)

  // the set of possible parameters
  implicit val thetas = Theta.Dom(fixedLengthSeqs(vectors(k),m),fixedLengthSeqs(vectors(k),n))

  // the set of possible cells
  implicit val cells = Cell.Dom(dom(0 until k),dom(0 until k))

  // positive training data, as a term
  val trainingData = fixedLengthSeq(IndexedSeq(Cell(0,0),Cell(1,0)))

  //call some user code to return a negative cell
  def sampleNegCell(posTerm:cells.Term) = for (pos <- posTerm) yield pos //do something to sample a different cell

  //the per cell loss
  def score(theta:thetas.Term)(cell:cells.Term) = theta.v(cell.i) dot theta.a(cell.j)

  //training loss, stochastic term based on sampling a positive cell, and then a negative based on it.
  def loss(t:thetas.Term) = {
    val pos = trainingData.sampleSequential
    val neg = sampleNegCell(pos)
    log(sigm(score(t)(pos))) - log(sigm(-score(t)(neg)))
  }


}
