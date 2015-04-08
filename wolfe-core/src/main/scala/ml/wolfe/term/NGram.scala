package ml.wolfe.term

import gnu.trove.map.hash.TIntDoubleHashMap

case class NGramCounts[E <: Dom, C <: TypedVectorDom[E]](data:SeqTerm[E], ngramOrder:Int)
                           (implicit countDom:C) extends Composed[C] {
  type ArgumentType = SeqTerm[E]

  val arguments = IndexedSeq(data)

  def copy(args: IndexedSeq[ArgumentType]) = ??? //NGramCounts(args(0),ngramOrder)(countDom)
  val domain = ???
}

object NGram {

  def ngramCounts[E](data: Seq[E], ngramOrder: Int)
                    (implicit ngramDom: VarSeqDom[TypedDom[E]],elemDom:TypedDom[E]): TIntDoubleHashMap = {
    import TermImplicits._
    val i = Ints(0 until data.length).Variable("i")
    val dataTerm = data.toConst
    val window = dataTerm.slice(i, i + ngramOrder)
    val eval = window.evaluatorImpl(window.createInputSettings())

    val map = new TIntDoubleHashMap(1000)
    for (i <- 0 until (data.length - ngramOrder + 1)) {
      eval.input(0).disc(0) = i
      eval.eval()(Execution(0))
      val index = ngramDom.indexOfSetting(eval.output)
      map.adjustOrPutValue(index, 1.0, 1.0)
    }
    map
  }

}