package ml.wolfe.term

import ml.wolfe.term

/**
 * @author riedel
 */
class VarSeqDom[+E <: Dom](val elementDom: E, val maxLength: Int, val minLength: Int = 0) extends Dom {

  dom =>

  val lengthDom = new DiscreteDom[Int](Range(minLength, maxLength))

  type Value = IndexedSeq[elementDom.Value]
  type Var = DomVar
  type Term = DomTerm
//  type ElemDom = E

  case class Marginals(length: lengthDom.Marginals, elements: IndexedSeq[elementDom.Marginals])

  def startOfElements(offsets: Offsets) = offsets.copy(discOff = offsets.discOff + 1)

  def toValue(setting: Setting, offsets: Offsets = Offsets()) = {
    val length = setting.disc(offsets.discOff)
    val start = startOfElements(offsets)
    val result = for (i <- 0 until length) yield elementDom.toValue(setting, start +(elementDom.lengths, i))
    result
  }

  def toMarginals(msg: Msgs, offsets: Offsets) = {
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


  def copyMarginals(marginals: Marginals, msgs: Msgs, offsets: Offsets) = {
    lengthDom.copyMarginals(marginals.length, msgs, offsets)
    val start = startOfElements(offsets)
    for (i <- 0 until maxLength) {
      elementDom.copyMarginals(marginals.elements(i), msgs, start +(elementDom.lengths, i))
    }
  }

  def fillZeroMsgs(target: Msgs, offsets: Offsets) = {
    lengthDom.fillZeroMsgs(target,offsets)
    val start = startOfElements(offsets)
    for (i <- 0 until maxLength) {
      elementDom.fillZeroMsgs(target, start +(elementDom.lengths, i))
    }
  }

  val lengths = (elementDom.lengths * maxLength) + Offsets(discOff = 1)

  def variable(name: String, staticOffsets: Offsets = Offsets(), owner: term.Var[Dom]): Var =
    new DomVar(name, staticOffsets,owner)

  def dynamic(name: => String, dynOffsets: => Offsets, owner: term.Var[Dom]): Var = ???

  def one = for (i <- 0 until minLength) yield elementDom.one

  def zero = for (i <- 0 until minLength) yield elementDom.zero

  def const(value: Value) = new Constructor(lengthDom.const(value.length),value.map(elementDom.const))

  trait DomTerm extends super.DomTerm {
    def apply(index:Int):term.Term[E]
    def length:TypedTerm[Int]
    def apply(index: term.Term[TypedDom[Int]]) =
      new VarSeqApply[E, Term, term.Term[TypedDom[Int]]](this, index)

  }

  class DomVar(name: =>String, val offsets:Offsets,owner:term.Var[Dom]) extends BaseVar(name,owner) with super.DomVar with DomTerm{
    def apply(index: Int) = new VarSeqApply[E,Term,lengthDom.Term](this,lengthDom.const(index))
    def length = new VarSeqLength[Term](this)
    def ranges = Ranges(offsets, startOfElements(offsets) +(domain.elementDom.lengths, domain.maxLength))
    def atomsIterator = ???
  }

  def Term(length:TypedTerm[Int],elements:IndexedSeq[elementDom.Term]):Term = new Constructor(length,elements)

  class Constructor(val length:TypedTerm[Int],val elements:IndexedSeq[term.Term[E]]) extends DomTerm with Composed[dom.type] {
    def apply(index:Int) = elements(index)

    type ArgumentType = term.Term[Dom]

    def arguments = length +: elements

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

  def differentiator(wrt: Seq[Var[Dom]]) = ???
}

class VarSeqApply[+E <: Dom, S <: Term[VarSeqDom[E]], I <: Term[TypedDom[Int]]](val seq: S, index: I) extends Composed[E] {
  self =>
  val domain = seq.domain.elementDom

  type ArgumentType = Term[Dom]

  def arguments = IndexedSeq(seq, index)


  def copy(args: IndexedSeq[ArgumentType]) = ???

  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      val index = inputs(1).disc(0)
      inputs(0).copyTo(output, domain.lengths, index, Offsets(), 0, domain.lengths, srcOffsets = Offsets(discOff = 1))
    }
  }

  def differentiator(wrt: Seq[Var[Dom]]) = new ComposedDifferentiator {
    require(index.vars.forall(v => !wrt.contains(v)), "Can't differentiate index term in sequence apply")
    //todo: should also check for length variable of sequence? no that should be done in seqApply

    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]) = {
      val index = argOutputs(1).disc(0)
      val length = argOutputs(0).disc(0)
      gradient(0) := 0.0
      gradient(0).disc(0) = length
      //update gradient at offset corresponding to index
      outError.copyTo(gradient(0), Offsets(), 0, domain.lengths, index, domain.lengths, tgtOffsets = Offsets(discOff = 1))
      //ranges.addInto(outError,gradient(0))
    }

    def withRespectTo = wrt
  }

}

class VarSeqConstructor[E<:Dom](val length:TypedTerm[Int],val elements:IndexedSeq[term.Term[E]]) extends Composed[VarSeqDom[E]] {
  def apply(index:Int) = elements(index)

  type ArgumentType = term.Term[Dom]

  def arguments = length +: elements

  val domain = new VarSeqDom[E](elements.head.domain,elements.size,0)

  def copy(args: IndexedSeq[ArgumentType]) = new VarSeqConstructor(
    args(0).asInstanceOf[TypedTerm[Int]],
    args.drop(1).asInstanceOf[IndexedSeq[Term[E]]])

  def composer() = new Evaluator {

    def eval(inputs: Array[Setting], output: Setting) = {
      //todo: adapt
      //todo: evaluate length first, and then only update the relevant elements
      val length = inputs(0).disc(0)
      for (i <- 0 until length) {
        inputs(i+1).copyTo(output, Offsets(), 0, domain.elementDom.lengths, i,domain.elementDom.lengths, tgtOffsets = Offsets(discOff = 1))
      }
      output.disc(0) = length
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
