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

