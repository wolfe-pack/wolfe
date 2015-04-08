package ml.wolfe.term

import cc.factorie.la.GrowableSparseHashTensor1
import ml.wolfe.term.TermImplicits._


case class NGramCounts[E <: Dom, C <: TypedVectorDom[VarSeqDom[E]]](data: SeqTerm[E], ngramOrder: Int)
                                                                   (implicit countsDom: C) extends Composed[C] {
  type ArgumentType = SeqTerm[E]

  val arguments = IndexedSeq(data)

  def copy(args: IndexedSeq[ArgumentType]) = NGramCounts[E, C](args(0), ngramOrder)(countsDom)

  val domain = countsDom

  override def composer(args: Settings) = new Composer(args) {

    val from = Ints(0 until data.domain.maxLength - ngramOrder + 1).Variable("from")
    val window = data.slice(from, from + ngramOrder)(countsDom.argDom)
    val fromVarIndex = window.vars.indexOf(from)
    val windowInput = args.linkedSettings(vars, window.vars)
    windowInput(fromVarIndex) = from.domain.createSetting()

    val windowEval = window.evaluatorImpl(windowInput)


    def eval()(implicit execution: Execution) = {
      val length = input(0).disc(0)
      val result = new GrowableSparseHashTensor1(0 until 1000)
      output.vect(0) = result
      for (i <- 0 until (length - ngramOrder + 1)) {
        windowEval.input(fromVarIndex).disc(0) = i
        windowEval.eval()
        val index = countsDom.argDom.indexOfSetting(windowEval.output)
        output.vect(0)(index) += 1.0
      }
    }
  }
}

trait NGramCountsHelper {
  def ngramCounts[E <: Dom, C <: TypedVectorDom[VarSeqDom[E]]](data: SeqTerm[E], ngramOrder: Int)
                                                              (implicit countsDom: C) = {
    NGramCounts[E,countsDom.type](data,ngramOrder)(countsDom)
  }
}