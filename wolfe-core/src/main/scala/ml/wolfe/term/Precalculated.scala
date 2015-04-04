package ml.wolfe.term

import cc.factorie.la._
import cc.factorie.util.SingletonIntSeq
import ml.wolfe.Index

/**
 * Precalculates the output of a static term. Creates a single shared evaluator.
 * @author riedel
 */
case class Precalculated[D <: Dom](term: Term[D]) extends Term[D] {

  require(term.isStatic)

  def vars = Seq.empty

  val domain = term.domain

  def isStatic = true

  val setting = term match {
//    case s: VarSeqDom[_]#Constructor if s.domain.elementDom.isInstanceOf[VectorDom] =>
//      //todo: this is a special which may have been slow in the generic match below.
//      val arguments = s.elements.asInstanceOf[IndexedSeq[Constant[VectorDom]]]
//      val result = new Setting(numDisc = 1, numVect = arguments.length)
//      result.disc(0) = arguments.length
//      for (i <- 0 until result.vect.length) result.vect(i) = arguments(i).value
//      result
    case t =>
      val eval = t.evaluatorImpl(Settings())
      eval.eval()(Execution(-1))
      eval.output
  }

  val eval = new AbstractEvaluator(Settings()) {
    def eval()(implicit execution: Execution) = {}

    val output = setting
  }


  override def evaluatorImpl(in: Settings) = eval
}






