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
  def toValue(setting: Setting, offsets: Offsets = Offsets()) = {
    val result = for (i <- 0 until length) yield elementDom.toValue(setting, offsets +(elementDom.lengths, i))
    result
  }
  def copyValue(value: Value, setting: Setting, offsets: Offsets = Offsets()) = {
    for (i <- 0 until length) {
      elementDom.copyValue(value(i), setting, offsets +(elementDom.lengths, i))
    }
  }
  val lengths = elementDom.lengths * length
  def variable(name: String, offsets: Offsets = Offsets(), owner: term.Var[Dom]) = StaticSeqVar(name, offsets, owner)

  def dynamic(name: => String, dynOffsets: => Offsets, owner: term.Var[Dom]) = new BaseVar(name, owner) with DomVar {
    def offsets = dynOffsets
    val elements:IndexedSeq[domain.elementDom.Var] = for (i <- 0 until domain.length) yield
      domain.elementDom.dynamic(s"$name($i)", offsets +(domain.elementDom.lengths, i), if (owner == null) this else owner)
  }

  def one = for (i <- 0 until length) yield elementDom.one
  def zero = for (i <- 0 until length) yield elementDom.zero

  def const(value: Value) = new SeqDomTermImpl {
    lazy val elements = for (i <- 0 until dom.length) yield domain.elementDom.const(value(i))
  }

  def Term(args: elementDom.Term*) = new SeqDomTermImpl {
    def elements = args.toIndexedSeq
  }
  def Const(args: elementDom.Value*) = new SeqDomTermImpl {
    def elements = args.map(a => elementDom.const(a)).toIndexedSeq
  }


  trait DomTerm extends super.DomTerm {
    def elements: IndexedSeq[domain.elementDom.DomTerm]

    def apply(index: Int) = elements(index)

    def indices = elements.indices
    def length = elements.length


  }

  abstract class SeqDomTermImpl extends Composed[dom.type] with DomTerm {
    def arguments = elements

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
    def atoms = elements.view.map(_.atoms).foldLeft(Atoms())(_ ++ _)
    def offsets: Offsets
    def ranges = Ranges(offsets, offsets +(domain.elementDom.lengths, domain.length))
    def apply(gen: Generator[Int]): domain.elementDom.DomVar = {
      domain.elementDom.dynamic(s"$name(${gen.current()}})", offsets +(domain.elementDom.lengths, gen.current()), if (owner == null) this else owner)
    }
  }

  case class StaticSeqVar(name: String, offsets: Offsets = Offsets(),
                          owner: term.Var[Dom]) extends DomVar {

    override val ranges = super.ranges
    val elements = for (i <- 0 until domain.length) yield
      domain.elementDom.variable(s"$name($i)", offsets +(domain.elementDom.lengths, i), if (owner == null) this else owner)

    /*
    def apply(generator:Generator[Int]) = new StochasticElement(generator) {
      val current = elements(generator.generate)

    }
    */


  }

  //val elements = arguments

}
