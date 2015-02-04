package ml.wolfe.term

import cc.factorie.la.{DenseTensor1, DenseTensor2}
import ml.wolfe._


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

  def Const(values: Double*) = {
    require(values.size == dim)
    const(new DenseVector(values.toArray))
  }

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
  def variable(name: String, offsets: Offsets = Offsets(), owner: term.Var[Dom]): Var = StaticDoubleVar(name, owner, offsets.contOff)

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





