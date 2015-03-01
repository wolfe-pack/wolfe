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
  //  type ElemDom = E

  val indexDom = new RangeDom(0 until maxLength)

  //trait Test extends Term
  def own(term: TypedTerm[Value]) = new ProxyTerm[TypedDom[Value]] with Term {
    def self = term

    def apply(index: Int) = ???

    def length = new VarSeqLength[Term](this)

    override val domain: dom.type = dom

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

  def variable(name: String, staticOffsets: Offsets = Offsets(), owner: term.Var[Dom]): Var =
    new DomVar(name, staticOffsets, owner)

  def dynamic(name: => String, dynOffsets: => Offsets, owner: term.Var[Dom]): Var = ???

  def one = for (i <- 0 until minLength) yield elementDom.one

  def zero = for (i <- 0 until minLength) yield elementDom.zero

  def Const(value: Value) = new Constructor(lengthDom.Const(value.length), value.map(elementDom.Const))

  trait DomTerm extends super.DomTerm {
    def apply(index: Int): term.Term[E]

    def length: TypedTerm[Int]

    def apply(index: term.Term[TypedDom[Int]]): elementDom.Term = {
      type Index = TypedTerm[Int]
      type ElemDom = TypedDom[elementDom.Value]
      type SeqTerm = term.Term[VarSeqDom[TypedDom[elementDom.Value]]]
      elementDom.own(new VarSeqApply[
        ElemDom, SeqTerm, Index](this.asInstanceOf[SeqTerm], index))
    }

    def sampleShuffled(implicit random: Random) = apply(indexDom.shuffled)

    def sampleSequential:elementDom.Term = apply(indexDom.sequential)

    def sampleUniform(implicit random: Random) = apply(indexDom.uniform)

  }

  class DomVar(name: => String, val offsets: Offsets, owner: term.Var[Dom]) extends BaseVar(name, owner) with super.DomVar with DomTerm {
    def apply(index: Int) = new VarSeqApply[E, Term, lengthDom.Term](this, lengthDom.Const(index))

    def length = new VarSeqLength[Term](this)

    def ranges = Ranges(offsets, startOfElements(offsets) +(domain.elementDom.lengths, domain.maxLength))

    def atomsIterator = ???
  }

  def Term(length: TypedTerm[Int], elements: IndexedSeq[elementDom.Term]): Term = new Constructor(length, elements)

  def Term(elements: elementDom.Term*): Term = Term(indexDom.Const(elements.size), elements.toIndexedSeq)

  class Constructor(val length: TypedTerm[Int], val elements: IndexedSeq[term.Term[E]]) extends DomTerm with Composed[dom.type] {
    def apply(index: Int) = elements(index)

    type ArgumentType = term.Term[Dom]

    val arguments = length +: elements

    def copy(args: IndexedSeq[ArgumentType]) = new Constructor(
      args(0).asInstanceOf[lengthDom.Term],
      args.drop(1).asInstanceOf[IndexedSeq[elementDom.Term]])

    def composer() = new Evaluator {

      def eval(inputs: Array[Setting], output: Setting) = {
        //todo: adapt
        //todo: evaluate length first, and then only update the relevant elements
        for (i <- 0 until inputs.length) {
          inputs(i).copyTo(output, domain.elementDom.lengths, i)
        }
      }
    }


    override def composer2(args: Settings) = new Composer2(args) {
      def eval()(implicit execution: Execution) = {
        output.disc(0) = input(0).disc(0)
        var offset = Offsets(discOff = 1)
        for (i <- 1 until input.length) {
          input(i).copyTo(output, Offsets.zero, offset, domain.elementDom.lengths)
          offset += domain.elementDom.lengths
        }
      }
    }


    override def differentiator2(wrt: Seq[term.Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) = {
      require(length.vars.forall(v => !wrt.contains(v)), "Can't differentiate length term in sequence constructor")
      new ComposedDifferentiator2(wrt, in, err, gradientAcc) {

        def localBackProp()(implicit execution: Execution) = {
          //each argument will get its error signal from a subsection of the outError
          val length = argOutputs(0).disc(0)
          var offsets = Offsets(discOff = 1)
          for (i <- 0 until length) {
            for (j <- 0 until domain.elementDom.lengths.contOff) {
              argErrors(i+1).cont(j) = error.cont(offsets.contOff + j)
            }
            for (j <- 0 until domain.elementDom.lengths.vectOff) {
              argErrors(i+1).vect(j) := error.vect(offsets.vectOff + j)
            }
            offsets += domain.elementDom.lengths
            //todo: matrices!
          }

        }
      }
    }

    def differentiator(wrt: Seq[term.Var[Dom]]) = new ComposedDifferentiator {
      //todo: adapt
      require(length.vars.forall(v => !wrt.contains(v)), "Can't differentiate length term in sequence constructor")

      def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]) = {
        //each argument will get its error signal from a subsection of the outError
        val length = argOutputs(0).disc(0)
        for (i <- 1 until length) {
          val offsets = (domain.elementDom.lengths * i) + Offsets(discOff = 1)
          for (j <- 0 until domain.elementDom.lengths.contOff) {
            gradient(i).cont(j) = outError.cont(offsets.contOff + j)
          }
          for (j <- 0 until domain.elementDom.lengths.vectOff) {
            gradient(i).vect(j) := outError.vect(offsets.vectOff + j)
          }
        }
      }

      def withRespectTo = wrt
    }
  }

}

class VarSeqLength[S <: Term[VarSeqDom[_]]](val seq: S) extends Composed[TypedDom[Int]] {
  type ArgumentType = S

  def copy(args: IndexedSeq[ArgumentType]) = new VarSeqLength[S](args(0))

  val arguments = IndexedSeq(seq)

  val domain: TypedDom[Int] = seq.domain.lengthDom


  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.disc(0) = inputs(0).disc(0)
    }
  }


  override def composer2(args: Settings) = new Composer2(args) {
    def eval()(implicit execution: Execution) = {
      output.disc(0) = input(0).disc(0)

    }
  }

  def differentiator(wrt: Seq[Var[Dom]]) = ???
}

class VarSeqApply[+E <: Dom, S <: Term[VarSeqDom[E]], I <: Term[TypedDom[Int]]](val seq: S, index: I) extends Composed[E] {
  self =>
  val domain = seq.domain.elementDom

  type ArgumentType = Term[Dom]

  val arguments = IndexedSeq(seq, index)


  def copy(args: IndexedSeq[ArgumentType]) = ???

  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      val index = inputs(1).disc(0)
      inputs(0).copyTo(output, domain.lengths, index, Offsets(), 0, domain.lengths, srcOffsets = Offsets(discOff = 1))
    }
  }


  override def composer2(args: Settings) = new Composer2(args) {
    def eval()(implicit execution: Execution) = {
      val index = input(1).disc(0)
      val offset = (seq.domain.elementDom.lengths * index) + Offsets(discOff = 1)
      output :=(input(0), offset, seq.domain.elementDom.lengths)
    }

  }


  override def differentiator2(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) =
    new ComposedDifferentiator2(wrt, in, err, gradientAcc) {
      argErrors(0).recordChangedOffsets = true

      def localBackProp()(implicit execution: Execution) = {
        val length = argOutputs(0).disc(0)
        val index = argOutputs(1).disc(0)
        val offset = seq.domain.elementDom.lengths * index + Offsets(discOff = 1)
        argErrors(0).resetToZero()
        argErrors(0).disc(0) = length
        error.copyTo(argErrors(0), Offsets.zero, offset, seq.domain.elementDom.lengths)
      }
    }

  def differentiator(wrt: Seq[Var[Dom]]) = new ComposedDifferentiator {
    require(index.vars.forall(v => !wrt.contains(v)), "Can't differentiate index term in sequence apply")

    //the indices of errors we pass to the sequence argument should be recorded such that the argument can focus on what was changed
    argErrors(0).recordChangedOffsets = true

    //todo: should also check for length variable of sequence? no that should be done in seqApply

    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]) = {
      gradient(0).resetToZero()
      val index = argOutputs(1).disc(0)
      val length = argOutputs(0).disc(0)
      gradient(0).disc(0) = length
      //update gradient at offset corresponding to index
      outError.copyTo(gradient(0), Offsets(), 0, domain.lengths, index, domain.lengths, tgtOffsets = Offsets(discOff = 1))
      //ranges.addInto(outError,gradient(0))
    }

    def withRespectTo = wrt
  }

}

