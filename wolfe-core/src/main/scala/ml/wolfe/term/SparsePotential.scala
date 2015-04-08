package ml.wolfe.term

import gnu.trove.map.hash.TIntDoubleHashMap

/**
 * @author riedel
 */
case class SparsePotential[D <: Dom](table: TIntDoubleHashMap, arg: Term[D]) extends ComposedDoubleTerm {
  type ArgumentType = Term[D]

  require(arg.domain.isDiscrete)

  val arguments = IndexedSeq(arg)

  def copy(args: IndexedSeq[ArgumentType]) = new SparsePotential[D](table, args(0))

  override def composer(args: Settings) = new Composer(args) {
    def eval()(implicit execution: Execution) = {
      val index = arg.domain.indexOfSetting(input(0))
      output.cont(0) = table.get(index)
    }
  }
}

object NGrams {

  def ngramCounts[E](data: Seq[E], ngramOrder: Int)
                    (implicit ngramDom: VarSeqDom[TypedDom[E]],elemDom:TypedDom[E]): TIntDoubleHashMap = {
    import TermImplicits._
    val i = Ints(0 until data.length).Var
    val dataTerm = data.toConst
    val window = dataTerm.slice(i, i + ngramOrder)
    val eval = window.evaluatorImpl(window.createInputSettings())

    val map = new TIntDoubleHashMap(1000)
    for (i <- 0 until (data.length - ngramOrder)) {
      eval.input(0).disc(0) = i
      eval.eval()(Execution(0))
      val index = ngramDom.indexOfSetting(eval.output)
      map.adjustOrPutValue(index, 1.0, 1.0)
    }
    map
  }

}