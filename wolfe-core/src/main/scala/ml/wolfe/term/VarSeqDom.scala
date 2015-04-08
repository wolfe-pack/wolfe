package ml.wolfe.term

import ml.wolfe.term

import scala.util.Random

/**
 * @author riedel
 */
class VarSeqDom[+E <: Dom](val elementDom: E, val maxLength: Int, val minLength: Int = 0) extends Dom {

  dom =>

  val lengthDom = new RangeDom(Range(minLength, maxLength + 1))

  type Value = IndexedSeq[elementDom.Value]
  type Var = DomVar
  type Term = DomTerm
  //type ElemDom = E

  def fixedSize = minLength == maxLength

  val indexDom = new RangeDom(0 until maxLength)

  //trait Test extends Term
  def own(term: TypedTerm[Value]) = new OwnedTerm[Value] with Term {
    def self = term

    def apply(index: Int) = new VarSeqApply[E, Term, lengthDom.Term](this, lengthDom.Const(index))

    def length = new VarSeqLength[Term](this)

    override val domain: dom.type = dom

    def copy(args: IndexedSeq[ArgumentType]) = own(args(0))

    def elements = for (i <- 0 until maxLength) yield apply(i)
  }

  case class Marginals(length: lengthDom.Marginals, elements: IndexedSeq[elementDom.Marginals])

  def startOfElements(offsets: Offsets) = offsets.copy(discOff = offsets.discOff + 1)

  def toValue(setting: Setting, offsets: Offsets = Offsets()) = {
    val length = setting.disc(offsets.discOff)
    val start = startOfElements(offsets)
    val result = for (i <- 0 until length) yield elementDom.toValue(setting, start +(elementDom.lengths, i))
    result
  }

  def toMarginals(msg: Msg, offsets: Offsets) = {
    val start = startOfElements(offsets)
    val length = lengthDom.toMarginals(msg, offsets)
    //todo: if length is deterministic, only collect marginals until actual length
    val elements = for (i <- 0 until maxLength) yield elementDom.toMarginals(msg, start +(elementDom.lengths, i))
    Marginals(length, elements)
  }

  def copyValue(value: Value, setting: Setting, offsets: Offsets = Offsets()) = {
    val length = value.length
    val start = startOfElements(offsets)
    for (i <- 0 until length) {
      elementDom.copyValue(value(i), setting, start +(elementDom.lengths, i))
    }
    lengthDom.copyValue(value.length, setting, offsets)
  }


  def copyMarginals(marginals: Marginals, msgs: Msg, offsets: Offsets) = {
    lengthDom.copyMarginals(marginals.length, msgs, offsets)
    val start = startOfElements(offsets)
    for (i <- 0 until maxLength) {
      elementDom.copyMarginals(marginals.elements(i), msgs, start +(elementDom.lengths, i))
    }
  }

  def fillZeroMsg(target: Msg, offsets: Offsets) = {
    lengthDom.fillZeroMsg(target, offsets)
    val start = startOfElements(offsets)
    for (i <- 0 until maxLength) {
      elementDom.fillZeroMsg(target, start +(elementDom.lengths, i))
    }
  }

  val lengths = (elementDom.lengths * maxLength) + Offsets(discOff = 1)
  override val dimensions = Dimensions(Array(minLength until maxLength + 1)) + elementDom.dimensions * maxLength

  def Variable(name: String): Var = new DomVar(name)


  def one = for (i <- 0 until minLength) yield elementDom.one

  def zero = for (i <- 0 until minLength) yield elementDom.zero

  def Const(value: Value) = new Constructor(lengthDom.Const(value.length), value.map(elementDom.Const))

  trait DomTerm extends super.DomTerm {
    def apply(index: Int): term.Term[E]

    def length: IntTerm

    def elements: IndexedSeq[term.Term[E]] // = for (i <- 0 until maxLength) yield apply(i)

    def apply(index: IntTerm): elementDom.Term = {
      type Index = IntTerm
      type ElemDom = TypedDom[elementDom.Value]
      type SeqTerm = term.Term[VarSeqDom[TypedDom[elementDom.Value]]]
      elementDom.own(new VarSeqApply[
        ElemDom, SeqTerm, Index](this.asInstanceOf[SeqTerm], index))
    }

    //todo: nasty, VarSeqDom[Dom] should be VarSeqDom[E] but that violates covariance
    //    def slice(from: IntTerm, to: IntTerm)(implicit sliceDom: VarSeqDom[elementDom.type]): sliceDom.Term = {
    //      val result = new VarSeqSlice[Dom, DomTerm, sliceDom.type](this, from, to)(sliceDom)
    //      sliceDom.own(result.asInstanceOf[TypedTerm[sliceDom.Value]])
    //    }

    def sampleShuffled(implicit random: Random) = apply(indexDom.shuffled)

    def sampleSequential: elementDom.Term = apply(indexDom.sequential)

    def sampleUniform(implicit random: Random) = apply(indexDom.uniform)

    //    def foldLeft[D : d.Term](d:Dom)(init:D#Term)(op:(elementDom.Term,D#Term) => D#Term) = ???
    //    def foldLeft[D <: Dom](init:D#Term)(op:(elementDom.Term,D#Term) => D#Term) = ???
    def foldLeft(init: term.Term[Dom])(op: (init.domain.Term, elementDom.Term) => init.domain.Term) = ???

  }

  class DomVar(name: String) extends BaseVar(name) with super.DomVar with DomTerm {
    def apply(index: Int) = new VarSeqApply[E, Term, lengthDom.Term](this, lengthDom.Const(index))

    def length = new VarSeqLength[Term](this)

    def elements = for (i <- 0 until maxLength) yield apply(i)


  }

  def Term(length: IntTerm, elements: IndexedSeq[elementDom.Term]): Term = new Constructor(length, elements)

  def Term(elements: elementDom.Term*): Term = Term(indexDom.Const(elements.size), elements.toIndexedSeq)

  class Constructor(val length: IntTerm, val elements: IndexedSeq[term.Term[E]]) extends DomTerm with Composed[dom.type] {
    def apply(index: Int) = elements(index)

    type ArgumentType = term.Term[Dom]

    val arguments = length +: elements

    def copy(args: IndexedSeq[ArgumentType]) = new Constructor(
      args(0).asInstanceOf[lengthDom.Term],
      args.drop(1).asInstanceOf[IndexedSeq[elementDom.Term]])


    override def composer(args: Settings) = new Composer(args) {
      output.recordChangedOffsets = true

      def eval()(implicit execution: Execution) = {
        output.clearChangeRecord()
        output.disc(0) = input(0).disc(0)
        var offset = Offsets(discOff = 1)
        for (i <- 1 until input.length) {
          input(i).copyTo(output, Offsets.zero, offset, domain.elementDom.lengths)
          offset += domain.elementDom.lengths
        }
      }
    }


    override def differentiatorImpl(wrt: Seq[term.Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) = {
      require(length.vars.forall(v => !wrt.contains(v)), "Can't differentiate length term in sequence constructor")
      new ComposedDifferentiator(wrt, in, err, gradientAcc) {

        def localBackProp()(implicit execution: Execution) = {
          //each argument will get its error signal from a subsection of the outError
          val length = argOutputs(0).disc(0)
          var offsets = Offsets(discOff = 1)
          for (i <- 0 until length) {
            for (j <- 0 until domain.elementDom.lengths.contOff) {
              argErrors(i + 1).cont(j) = error.cont(offsets.contOff + j)
            }
            for (j <- 0 until domain.elementDom.lengths.vectOff) {
              argErrors(i + 1).vect(j) := error.vect(offsets.vectOff + j)
            }
            offsets += domain.elementDom.lengths
            //todo: matrices!
          }

        }
      }
    }


    override def toString = s"""ISeq($length)(${elements.mkString(",")})"""
  }

  override def toString = s"Seqs($elementDom,$minLength,$maxLength)"

}

case class VarSeqLength[S <: Term[VarSeqDom[_]]](seq: S) extends Composed[IntDom] {
  type ArgumentType = S

  def copy(args: IndexedSeq[ArgumentType]) = new VarSeqLength[S](args(0))

  val arguments = IndexedSeq(seq)

  val domain: IntDom = seq.domain.lengthDom


  override def composer(args: Settings) = new Composer(args) {
    def eval()(implicit execution: Execution) = {
      output.disc(0) = input(0).disc(0)

    }
  }

  def differentiatorOld(wrt: Seq[Var[Dom]]) = ???
}

case class VarSeqApply[+E <: Dom, S <: Term[VarSeqDom[E]], I <: IntTerm](seq: S, index: I) extends Composed[E] {
  self =>
  val domain = seq.domain.elementDom

  type ArgumentType = Term[Dom]

  val arguments = IndexedSeq(seq, index)


  def copy(args: IndexedSeq[ArgumentType]) = VarSeqApply[E, S, I](args(0).asInstanceOf[S], args(1).asInstanceOf[I])


  override def composer(args: Settings) = new Composer(args) {
    def eval()(implicit execution: Execution) = {
      val index = input(1).disc(0)
      val offset = (seq.domain.elementDom.lengths * index) + Offsets(discOff = 1)
      output :=(input(0), offset, seq.domain.elementDom.lengths)
    }

  }


  override def differentiatorImpl(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) =
    new ComposedDifferentiator(wrt, in, err, gradientAcc) {
      argErrors(0).recordChangedOffsets = true

      def localBackProp()(implicit execution: Execution) = {
        val length = argOutputs(0).disc(0)
        val index = argOutputs(1).disc(0)
        val offset = seq.domain.elementDom.lengths * index + Offsets(discOff = 1)
        argErrors(0).resetToZero()
        argErrors(0).disc := argOutputs(0).disc
        error.copyTo(argErrors(0), Offsets.zero, offset, seq.domain.elementDom.lengths)
      }
    }


  override def toString = s"$seq($index)"
}

case class VarSeqSlice[+E <: Dom, S <: Term[VarSeqDom[E]], D <: VarSeqDom[E]](seq: S, from: IntTerm, to: IntTerm)
                                                                             (implicit sliceDom: D) extends Composed[D] {
  self =>
  val domain = sliceDom

  type ArgumentType = Term[Dom]

  val arguments = IndexedSeq(seq, from, to)


  def copy(args: IndexedSeq[ArgumentType]) =
    VarSeqSlice[E, S, D](args(0).asInstanceOf[S], args(1).asInstanceOf[IntTerm], args(2).asInstanceOf[IntTerm])

  override def composer(args: Settings) = new Composer(args) {
    val tgtOffsets = Offsets(discOff = 1)

    def eval()(implicit execution: Execution) = {
      val from = input(1).disc(0)
      val to = input(2).disc(0)
      val offset = (seq.domain.elementDom.lengths * from) + Offsets(discOff = 1)
      val length = seq.domain.elementDom.lengths * (to - from)
      output :=(input(0), offset, length, tgtOffsets)
      output.disc(0) = to - from
    }
  }

  override def toString = s"$seq.slice($from,$to)"
}


class RangeTerm(start: IntTerm, end: IntTerm) extends Composed[VarSeqDom[IntDom]] {


  type ArgumentType = IntTerm

  def arguments = IndexedSeq(start, end)

  def copy(args: IndexedSeq[ArgumentType]) = new RangeTerm(args(0), args(1))

  val startMin = start.domain.start
  val startMax = start.domain.end - 1
  val endMin = end.domain.start
  val endMax = end.domain.end - 1
  val range = startMin until endMax

  val domain: VarSeqDom[IntDom] = new VarSeqDom(RangeDom(range), endMax - startMin, endMin - startMax)

  override def composer(args: Settings) = new Composer(args) {
    def eval()(implicit execution: Execution) = {
      val from = input(0).disc(0)
      val to = input(1).disc(0)
      val length = to - from
      output.disc(0) = length
      for (i <- 0 until length) output.disc(1 + i) = from + i
    }
  }


}