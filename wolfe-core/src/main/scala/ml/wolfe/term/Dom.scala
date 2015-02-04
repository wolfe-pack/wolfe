package ml.wolfe.term

import cc.factorie.la.{SparseIndexedTensor2, DenseTensor2, DenseTensor1}
import ml.wolfe._
import ml.wolfe.fg20.Msgs


trait Dom {

  dom =>

  type Value
  trait DomTerm extends term.Term[dom.type] {
    val domain: dom.type = dom
  }
  trait DomVar extends term.Var[dom.type] with DomTerm

  type Var <: DomVar
  type Term <: DomTerm

  def toValue(setting: Setting, offsets: Offsets = Offsets()): Value
  def copyValue(value: Value, setting: Setting, offsets: Offsets = Offsets())

  def toSetting(value: Value): Setting = {
    val result = createSetting()
    copyValue(value, result)
    result
  }
  def createSetting(): Setting = new Setting(lengths.discOff, lengths.contOff, lengths.vectOff, lengths.matsOff)
  def createMsgs() = new Msgs(null, null, null)
  def createZeroSetting(): Setting = {
    val result = createSetting()
    copyValue(zero, result)
    result
  }
  def variable(name: String, offsets: Offsets = Offsets(), owner: term.Var[Dom] = null): Var
  def dynamic(name: => String, offsets: => Offsets = Offsets(), owner: term.Var[Dom] = null): Var
  def const(value: Value): Term

  def lengths: Offsets

  def isDiscrete = lengths.contOff == 0 && lengths.vectOff == 0 && lengths.matsOff == 0
  def isContinuous = lengths.discOff == 0
  def isDouble = lengths.contOff == 1 && lengths.discOff == 0 && lengths.vectOff == 0 && lengths.matsOff == 0


  def one: Value
  def zero: Value

  abstract class BaseVar(dynName: => String, val owner: term.Var[Dom]) extends DomVar {
    def name = dynName
  }

  class Constant(val value: Value) extends DomTerm {
    self =>

    val vars      = Seq.empty
    val evaluator = new Evaluator {
      val result = domain.toSetting(value.asInstanceOf[domain.Value])

      def eval(inputs: Array[Setting], output: Setting) = {
        output := result
      }
    }

    def atoms = Atoms()

    def differentiator(wrt: Seq[term.Var[Dom]]) = new Differentiator {
      val result = domain.toSetting(value.asInstanceOf[domain.Value])

      def forwardProp(current: Array[Setting]) = activation := result

      def term = self

      def withRespectTo = wrt

      def backProp(error: Setting, gradient: Array[Setting]) = {}
    }
  }


}

object Dom {
  val doubles = new DoubleDom
  val bools   = TermImplicits.discrete(false, true)
}

class VectorDom(val dim: Int) extends Dom {

  dom =>

  type Value = FactorieVector
  type Var = DomVar
  type Term = DomTerm

  val lengths = Offsets(0, 0, 1, 0)

  def toValue(setting: Setting, offsets: Offsets) =
    setting.vect(offsets.vectOff)
  def copyValue(value: Value, setting: Setting, offsets: Offsets) =
    setting.vect(offsets.vectOff) = value
  def variable(name: String, offsets: Offsets, owner: term.Var[Dom]) = StaticVectorVar(name, owner, offsets.vectOff)

  def dynamic(name: => String, offsets: => Offsets, owner: term.Var[Dom]) = new BaseVar(name, owner) with DomVar {
    def offset = offsets.vectOff
  }
  def one = new DenseTensor1(dim, 1.0)
  def zero = new DenseTensor1(dim, 0.0)

  def const(value: Value) = new Constant(value)

  def Const(values:Double*) = const(new DenseVector(values.toArray))

  trait DomVar extends Atom with DomTerm with super.DomVar {
    def ranges = Ranges(Offsets(0, 0, offset, 0), Offsets(0, 0, offset + 1, 0))
    def atoms = Atoms(vect = List(this))
  }

  case class StaticVectorVar(name: String, owner: term.Var[Dom], offset: Int) extends DomVar with Atom {
    override val ranges = super.ranges

  }

}
class MatrixDom(val dim1: Int, dim2: Int) extends Dom {
  dom =>
  type Value = FactorieMatrix
  type Var = DomVar
  type Term = DomTerm

  val lengths = Offsets(0, 0, 0, 1)

  def toValue(setting: Setting, offsets: Offsets) =
    setting.mats(offsets.matsOff)
  def copyValue(value: Value, setting: Setting, offsets: Offsets) =
    setting.mats(offsets.matsOff) = value
  def variable(name: String, offsets: Offsets, owner: term.Var[Dom]): DomVar = DomVar(name, owner, offsets.matsOff)

  def dynamic(name: => String, offsets: => Offsets, owner: term.Var[Dom]) = ???
  def one = new DenseTensor2(dim1, dim2, 1.0)
  def zero = new DenseTensor2(dim1, dim2, 0.0)

  def const(value: Value) = new Constant(value)

  case class DomVar(name: String, owner: term.Var[Dom], offset: Int) extends DomTerm with Atom with super.DomVar {
    val ranges = Ranges(Offsets(0, 0, 0, offset), Offsets(0, 0, 0, offset + 1))
    def atoms = Atoms(mats = List(this))
  }

}

class DoubleDom extends Dom {
  dom =>
  type Value = Double

  type Var = DomVar
  type Term = DomTerm

  def toValue(setting: Setting, offsets: Offsets = Offsets()) =
    setting.cont(offsets.contOff)
  def copyValue(value: Value, setting: Setting, offsets: Offsets = Offsets()) =
    setting.cont(offsets.contOff) = value
  val lengths = Offsets(0, 1, 0, 0)
  def variable(name: String, offsets: Offsets = Offsets(), owner: term.Var[Dom]):Var = StaticDoubleVar(name, owner, offsets.contOff)

  def dynamic(name: => String, offsets: => Offsets, owner: term.Var[Dom]) = new BaseVar(name, owner) with DomVar {
    def offset = offsets.contOff
  }
  def one = 1.0
  def zero = 0.0
  def const(value: Value) = new Constant(value)


  trait DomVar extends Atom with super.DomVar {
    self =>
    def ranges = Ranges(Offsets(0, offset, 0, 0), Offsets(0, offset + 1, 0, 0))

    //val domain:self.type = self
    override def argmaxer(wrt: Seq[term.Var[Dom]]) = new Argmaxer {
      val contained = wrt.contains(self)

      def argmax(observed: Array[Setting], msgs: Array[Msgs], result: Array[Setting]) = {
        result(0).cont(0) = if (contained) Double.PositiveInfinity else observed(0).cont(0)
      }
    }
    def atoms = Atoms(cont = List(this))
  }

  case class StaticDoubleVar(name: String, owner: term.Var[Dom], offset: Int) extends Var with DomTerm {
    override val ranges = super.ranges
  }
}

class DiscreteDom[T](val values: IndexedSeq[T]) extends Dom {



  dom =>
  type Term = DomTerm
  type Value = T
  type Var = DomVar
  def toValue(setting: Setting, offsets: Offsets = Offsets()) =
    values(setting.disc(offsets.discOff))
  def copyValue(value: Value, setting: Setting, offsets: Offsets = Offsets()) =
    setting.disc(offsets.discOff) = values.indexOf(value)
  val lengths = Offsets(1, 0, 0, 0)
  def variable(name: String, offsets: Offsets = Offsets(), owner: term.Var[Dom]) = StaticDiscVar(name, owner, offsets.discOff)

  def dynamic(name: => String, offsets: => Offsets, owner: term.Var[Dom]) = new BaseVar(name, owner) with DomVar {
    def offset = offsets.discOff
  }

  def one = values.last
  def zero = values.head
  def const(value: T) = new Constant(value)


  trait DomVar extends Atom with super.DomVar {
    def ranges = Ranges(Offsets(offset, 0, 0, 0), Offsets(offset + 1, 0, 0, 0))
    def atoms = Atoms(disc = List(this.asInstanceOf[DiscVar[Any]]))
  }

  case class StaticDiscVar(name: String, owner: term.Var[Dom], offset: Int) extends DomVar {
    override val ranges = super.ranges

  }


}


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

class Tuple2Dom[D1 <: Dom, D2 <: Dom](val dom1: D1, val dom2: D2) extends Dom {
  dom =>
  type Value = (dom1.Value, dom2.Value)
  type Var = DomVar
  type Term = DomTerm
  val lengths = dom1.lengths + dom2.lengths
  def toValue(setting: Setting, offsets: Offsets = Offsets()) = {
    val arg1 = dom1.toValue(setting, offsets)
    val arg2 = dom2.toValue(setting, offsets + dom1.lengths)
    (arg1, arg2)
  }
  def copyValue(value: Value, setting: Setting, offsets: Offsets = Offsets()): Unit = {
    dom1.copyValue(value._1, setting)
    dom2.copyValue(value._2, setting, dom1.lengths)
  }
  def variable(name: String, offsets: Offsets, owner: term.Var[Dom]): DomVar =
    StaticTuple2Var(name, offsets, owner)

  def dynamic(name: => String, dynOffsets: => Offsets, owner: term.Var[Dom]): DomVar = new BaseVar(name, owner) with DomVar {
    def offsets = dynOffsets
    val _1 = domain.dom1.dynamic(name + "._1", offsets, if (owner == null) this else owner)
    val _2 = domain.dom2.dynamic(name + "._2", offsets + domain.dom1.lengths, if (owner == null) this else owner)
  }
  def one = (dom1.one, dom2.one)
  def zero = (dom1.zero, dom2.zero)

  def const(value: Value): DomTerm = new Tuple2DomTermImpl {
    val _1 = domain.dom1.const(value._1)
    val _2 = domain.dom2.const(value._2)
  }

  trait DomTerm extends super.DomTerm {
    def _1: domain.dom1.Term
    def _2: domain.dom2.Term
  }

  trait Tuple2DomTermImpl extends DomTerm with Composed[dom.type] {
    def arguments = IndexedSeq(_1, _2)

    def composer() = ???

    def differentiator(wrt: Seq[term.Var[Dom]]) = ???
  }

  trait DomVar extends super.DomVar {
    def offsets: Offsets
    def ranges = Ranges(offsets, offsets + domain.dom1.lengths + domain.dom2.lengths)
    def atoms = _1.atoms ++ _2.atoms
    def _1: domain.dom1.Var
    def _2: domain.dom2.Var


  }

  case class StaticTuple2Var(name: String,
                             offsets: Offsets,
                             owner: term.Var[Dom]) extends DomVar {
    override val ranges = super.ranges
    val _1 = domain.dom1.variable(name + "._1", offsets, if (owner == null) this else owner)
    val _2 = domain.dom2.variable(name + "._2", offsets + domain.dom1.lengths, if (owner == null) this else owner)

  }

}
