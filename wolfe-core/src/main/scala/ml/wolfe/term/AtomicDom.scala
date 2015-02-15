package ml.wolfe.term

import cc.factorie.la.{DenseTensor1, DenseTensor2}
import ml.wolfe._

/**
 * @author riedel
 */
trait AtomicDom extends Dom {
  type Term = DomTerm
  def const(value: Value) = new Constant(value)

}

class VectorDom(val dim: Int) extends Dom {

  dom =>

  type Value = FactorieVector
  type Var = DomVar
  type Term = DomTerm
  type Marginals = FactorieVector

  val lengths = Offsets(0, 0, 1, 0)

  def toValue(setting: Setting, offsets: Offsets) =
    setting.vect(offsets.vectOff)


  def toMarginals(msg: Msgs, offsets: Offsets) = {
    msg.vect(offsets.vectOff).mean
  }

  def fillZeroMsgs(target: Msgs, offsets: Offsets) = {
    target.vect(offsets.vectOff) = new VectMsg(one)
  }

  def copyValue(value: Value, setting: Setting, offsets: Offsets) =
    setting.vect(offsets.vectOff) = value


  def copyMarginals(marginals: Marginals, msgs: Msgs, offsets: Offsets) = {
    msgs.vect(offsets.vectOff) = new VectMsg(marginals)
  }
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

  trait DomVar extends Atom[dom.type] with DomTerm with super.DomVar {
    def ranges = Ranges(Offsets(0, 0, offset, 0), Offsets(0, 0, offset + 1, 0))
  }

  case class StaticVectorVar(name: String, owner: term.Var[Dom], offset: Int) extends DomVar with Atom[dom.type] {
    override val ranges = super.ranges

  }

}
class MatrixDom(val dim1: Int, dim2: Int) extends Dom {
  dom =>
  type Value = FactorieMatrix
  type Var = DomVar
  type Term = DomTerm
  type Marginals = FactorieMatrix

  val lengths = Offsets(0, 0, 0, 1)

  def toValue(setting: Setting, offsets: Offsets) =
    setting.mats(offsets.matsOff)

  def toMarginals(msg: Msgs, offsets: Offsets) = {
    msg.mats(offsets.matsOff).mean
  }

  def copyValue(value: Value, setting: Setting, offsets: Offsets) =
    setting.mats(offsets.matsOff) = value


  def copyMarginals(marginals: Marginals, msgs: Msgs, offsets: Offsets) = {
    msgs.mats(offsets.matsOff) = new MatsMsg(marginals)
  }

  def fillZeroMsgs(target: Msgs, offsets: Offsets) = {
    target.mats(offsets.matsOff) = new MatsMsg(zero)
  }

  def variable(name: String, offsets: Offsets, owner: term.Var[Dom]): DomVar = DomVar(name, owner, offsets.matsOff)

  def dynamic(name: => String, offsets: => Offsets, owner: term.Var[Dom]) = ???
  def one = new DenseTensor2(dim1, dim2, 1.0)
  def zero = new DenseTensor2(dim1, dim2, 0.0)

  def const(value: Value) = new Constant(value)

  case class DomVar(name: String, owner: term.Var[Dom], offset: Int) extends DomTerm with Atom[dom.type] with super.DomVar {
    val ranges = Ranges(Offsets(0, 0, 0, offset), Offsets(0, 0, 0, offset + 1))
  }

}



class DoubleDom extends AtomicDom {
  dom =>
  type Value = Double
  type Var = DomVar
  type Marginals = Double

  def toValue(setting: Setting, offsets: Offsets = Offsets()) =
    setting.cont(offsets.contOff)


  def toMarginals(msg: Msgs, offsets: Offsets) = {
    msg.cont(offsets.contOff).mean
  }

  def copyValue(value: Value, setting: Setting, offsets: Offsets = Offsets()) =
    setting.cont(offsets.contOff) = value


  def copyMarginals(marginals: Marginals, msgs: Msgs, offsets: Offsets) = {
    msgs.cont(offsets.contOff) = new ContMsg(marginals)
  }

  def fillZeroMsgs(target: Msgs, offsets: Offsets) = {
    target.cont(offsets.contOff) = new ContMsg(0.0)
  }

  val lengths = Offsets(0, 1, 0, 0)
  def variable(name: String, offsets: Offsets = Offsets(), owner: term.Var[Dom]): Var = StaticDoubleVar(name, owner, offsets.contOff)

  def dynamic(name: => String, offsets: => Offsets, owner: term.Var[Dom]):Var = new BaseVar(name, owner) with DomVar {
    def offset = offsets.contOff
  }
  def one = 1.0
  def zero = 0.0

  trait DomVar extends Atom[dom.type] with super.DomVar {
    self =>
    def ranges = Ranges(Offsets(0, offset, 0, 0), Offsets(0, offset + 1, 0, 0))

    //val domain:self.type = self
    override def argmaxer(wrt: Seq[term.Var[Dom]]) = new Argmaxer {
      val contained = wrt.contains(self)

      def argmax(observed: Array[Setting], msgs: Array[Msgs], result: Array[Setting]) = {
        result(0).cont(0) = if (contained) Double.PositiveInfinity else observed(0).cont(0)
      }
    }
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
  type Marginals = IndexedSeq[Double]
  def toValue(setting: Setting, offsets: Offsets = Offsets()) =
    values(setting.disc(offsets.discOff))

  def toMarginals(msg: Msgs, offsets: Offsets) = {
    msg.disc(offsets.discOff).msg
  }
  def copyValue(value: Value, setting: Setting, offsets: Offsets = Offsets()) =
    setting.disc(offsets.discOff) = values.indexOf(value)


  def copyMarginals(marginals: Marginals, msgs: Msgs, offsets: Offsets) = {
    msgs.disc(offsets.discOff) = new DiscMsg(marginals.toArray)
  }
  def fillZeroMsgs(target: Msgs, offsets: Offsets) = {
    target.disc(offsets.discOff) = new DiscMsg(values.length)
  }

  val lengths = Offsets(1, 0, 0, 0)
  def variable(name: String, offsets: Offsets = Offsets(), owner: term.Var[Dom]) = StaticDiscVar(name, owner, offsets.discOff)

  def dynamic(name: => String, offsets: => Offsets, owner: term.Var[Dom]) = new BaseVar(name, owner) with DomVar {
    def offset = offsets.discOff
  }

  def one = values.last
  def zero = values.head
  def const(value: T) = new Constant(value)


  trait DomVar extends Atom[dom.type] with super.DomVar {
    def ranges = Ranges(Offsets(offset, 0, 0, 0), Offsets(offset + 1, 0, 0, 0))
  }

  case class StaticDiscVar(name: String, owner: term.Var[Dom], offset: Int) extends DomVar {
    override val ranges = super.ranges

  }

}

