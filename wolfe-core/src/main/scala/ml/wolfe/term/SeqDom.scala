package ml.wolfe.term

import ml.wolfe.term

/**
 * @author riedel
 */
class SeqDom[D <: Dom](val elementDom: D, val length: Int) extends Dom {

  dom =>

  type Value = IndexedSeq[elementDom.Value]
  type Var = DomVar
  type Term = DomTerm
  type Marginals = IndexedSeq[elementDom.Marginals]

  def toValue(setting: Setting, offsets: Offsets = Offsets()) = {
    val result = for (i <- 0 until length) yield elementDom.toValue(setting, offsets +(elementDom.lengths, i))
    result
  }


  def toMarginals(msg: Msgs, offsets: Offsets) = {
    for (i <- 0 until length) yield elementDom.toMarginals(msg, offsets +(elementDom.lengths, i))
  }
  def copyValue(value: Value, setting: Setting, offsets: Offsets = Offsets()) = {
    for (i <- 0 until length) {
      elementDom.copyValue(value(i), setting, offsets +(elementDom.lengths, i))
    }
  }


  def copyMarginals(marginals: Marginals, msgs: Msgs, offsets: Offsets) = {
    for (i <- 0 until length) {
      elementDom.copyMarginals(marginals(i), msgs, offsets +(elementDom.lengths, i))
    }
  }

  def fillZeroMsgs(target: Msgs, offsets: Offsets) = {
    for (i <- 0 until length) {
      elementDom.fillZeroMsgs(target, offsets +(elementDom.lengths, i))
    }
  }

  val lengths = elementDom.lengths * length
  def variable(name: String, staticOffsets: Offsets = Offsets(), owner: term.Var[Dom]):Var = new BaseVar(name, owner) with DomVar {
    val offsets = staticOffsets
  }

  def dynamic(name: => String, dynOffsets: => Offsets, owner: term.Var[Dom]):Var = new BaseVar(name, owner) with DomVar {
    def offsets = dynOffsets
  }

  def one = for (i <- 0 until length) yield elementDom.one
  def zero = for (i <- 0 until length) yield elementDom.zero

  def const(value: Value) = new SeqDomTermImpl {
    lazy val elements = for (i <- 0 until dom.length) yield domain.elementDom.const(value(i))

    def copy(args: IndexedSeq[ArgumentType]) = Term(args:_*)
  }

  def Term(args: elementDom.Term*):Term = new SeqDomTermImpl {
    def elements = args.toIndexedSeq

    def copy(args: IndexedSeq[ArgumentType]) = Term(args:_*)
  }
  def Const(args: elementDom.Value*) = new SeqDomTermImpl {
    def elements = args.map(a => elementDom.const(a)).toIndexedSeq

    def copy(args: IndexedSeq[ArgumentType]) = Term(args:_*)
  }


  trait DomTerm extends super.DomTerm {
    def apply(index: =>Int):elementDom.Term // = elements(index)
    def indices = Range(0,dom.length)
    def length = dom.length
  }

  abstract class SeqDomTermImpl extends Composed[dom.type] with DomTerm {

    type ArgumentType = domain.elementDom.Term

    def arguments = elements
    def elements: IndexedSeq[domain.elementDom.Term]
    def apply(index: => Int) = elements(index)

    def composer() = new Evaluator {

      def eval(inputs: Array[Setting], output: Setting) = {
        for (i <- 0 until inputs.length) {
          inputs(i).copyTo(output, domain.elementDom.lengths, i)
        }
      }
    }

    def differentiator(wrt: Seq[term.Var[Dom]]) = new ComposedDifferentiator {
      def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]) = {
        //each argument will get its error signal from a subsection of the outError
        for (i <- 0 until argOutputs.length) {
          val offsets = domain.elementDom.lengths * i
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

  trait DomVar extends DomTerm with super.DomVar {
    def elements = indices.map(i => apply(i))
    def apply(index: =>Int): domain.elementDom.Var = {
      domain.elementDom.dynamic(s"$name($index})", offsets +(domain.elementDom.lengths, index), if (owner == null) this else owner)
    }
    def apply(index: Dynamic[Int]): domain.elementDom.Var = {
      domain.elementDom.dynamic(s"$name(${index.value()})", offsets +(domain.elementDom.lengths, index.value()), if (owner == null) this else owner)
    }


    def atomsIterator = elements.iterator.flatMap(_.atomsIterator)

    def offsets: Offsets
    def ranges = Ranges(offsets, offsets +(domain.elementDom.lengths, domain.length))
    def static(gen: Int): domain.elementDom.Var = {
      domain.elementDom.variable(s"$name($gen})", offsets +(domain.elementDom.lengths, gen), if (owner == null) this else owner)
    }
  }

//  case class StaticSeqVar(name: String, offsets: Offsets = Offsets(),
//                          owner: term.Var[Dom]) extends DomVar {
//
//    override val ranges = super.ranges
//    val elements = for (i <- 0 until domain.length) yield
//      domain.elementDom.variable(s"$name($i)", offsets +(domain.elementDom.lengths, i), if (owner == null) this else owner)
//
//    /*
//    def apply(generator:Generator[Int]) = new StochasticElement(generator) {
//      val current = elements(generator.generate)
//
//    }
//    */
//
//
//  }

  //val elements = arguments

}
