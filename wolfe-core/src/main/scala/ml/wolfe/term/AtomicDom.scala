package ml.wolfe.term

import cc.factorie.la.{DenseTensor1, DenseTensor2}
import ml.wolfe._
import ml.wolfe.util.Math._

import scala.util.Random

/**
 * @author riedel
 */
trait AtomicDom extends Dom {

  dom =>
  type Term = DomTerm

  def Const(value: Value) = new Constant(value)

  def own(term: TypedTerm[Value]) = new OwnedTerm[Value] with Term {
    def self = term

    override val domain: dom.type = dom

    def copy(args: IndexedSeq[ArgumentType]) = own(args(0))
  }
}

trait GenericVectorDom extends AtomicDom {
  dom =>

  def dim: Int

  type Value = FactorieVector
  type Var = DomVar
  type Marginals = FactorieVector

  val lengths = Offsets(0, 0, 1, 0)

  def toValue(setting: Setting, offsets: Offsets) =
    setting.vect(offsets.vectOff)


  def toMarginals(msg: Msg, offsets: Offsets) = {
    msg.vect(offsets.vectOff).mean
  }

  def fillZeroMsg(target: Msg, offsets: Offsets) = {
    target.vect(offsets.vectOff) = new VectMsg(one)
  }

  def copyValue(value: Value, setting: Setting, offsets: Offsets) =
    setting.vect(offsets.vectOff) = value


  def copyMarginals(marginals: Marginals, msgs: Msg, offsets: Offsets) = {
    msgs.vect(offsets.vectOff) = new VectMsg(marginals)
  }

  def one = new DenseTensor1(dim, 1.0)

  def zero = new DenseTensor1(dim, 0.0)

  def dynConst(value: Dynamic[Value]) = new DynamicConstant(value)

  def Term(values: Double*) = {
    require(values.size == dim)
    Const(new DenseVector(values.toArray))
  }

  trait DomVar extends Atom[dom.type] with DomTerm with super.DomVar {
    def ranges = Ranges(Offsets(0, 0, offset, 0), Offsets(0, 0, offset + 1, 0))
  }

  case class StaticVectorVar(name: String, owner: term.Var[Dom], offset: Int) extends DomVar {
    override val ranges = super.ranges
  }


}

class VectorDom(val dim: Int) extends GenericVectorDom {

  def variable(name: String, offsets: Offsets, owner: term.Var[Dom]) = StaticVectorVar(name, owner, offsets.vectOff)

  def dynamic(name: => String, offsets: => Offsets, owner: term.Var[Dom]) = new BaseVar(name, owner) with DomVar {
    def offset = offsets.vectOff
  }

}

class UnitVectorDom(val dim: Int) extends GenericVectorDom {
  dom =>

  trait UnitVectorVar extends Atom[dom.type] {
    override def projectValue(setting: Setting) = {
      setting.vect(offset).normalize()
    }
  }

  def variable(name: String, offsets: Offsets, owner: term.Var[Dom]) =
    new StaticVectorVar(name, owner, offsets.vectOff) with UnitVectorVar

  def dynamic(name: => String, offsets: => Offsets, owner: term.Var[Dom]) = new BaseVar(name, owner) with DomVar with UnitVectorVar {
    def offset = offsets.vectOff
  }

}


class MatrixDom(val dim1: Int, dim2: Int) extends AtomicDom {
  dom =>
  type Value = FactorieMatrix
  type Var = DomVar
  type Marginals = FactorieMatrix

  val lengths = Offsets(0, 0, 0, 1)

  def toValue(setting: Setting, offsets: Offsets) =
    setting.mats(offsets.matsOff)

  def toMarginals(msg: Msg, offsets: Offsets) = {
    msg.mats(offsets.matsOff).mean
  }

  def copyValue(value: Value, setting: Setting, offsets: Offsets) =
    setting.mats(offsets.matsOff) = value


  def copyMarginals(marginals: Marginals, msgs: Msg, offsets: Offsets) = {
    msgs.mats(offsets.matsOff) = new MatsMsg(marginals)
  }

  def fillZeroMsg(target: Msg, offsets: Offsets) = {
    target.mats(offsets.matsOff) = new MatsMsg(zero)
  }

  def variable(name: String, offsets: Offsets, owner: term.Var[Dom]): DomVar = new StaticMatrixVar(name, owner, offsets.matsOff)

  def dynamic(name: => String, offsets: => Offsets, owner: term.Var[Dom]) = new BaseVar(name, owner) with DomVar {
    def offset = offsets.matsOff
  }

  def one = new DenseTensor2(dim1, dim2, 1.0)

  def zero = new DenseTensor2(dim1, dim2, 0.0)

  case class StaticMatrixVar(name: String, owner: term.Var[Dom], offset: Int) extends DomVar {
    override val ranges = super.ranges
  }

  trait DomVar extends DomTerm with Atom[dom.type] with super.DomVar {
    def ranges = Ranges(Offsets(0, 0, 0, offset), Offsets(0, 0, 0, offset + 1))
  }

}

class BooleanDom extends GenericDiscreteDom[Boolean] {

  def domainSize = 2

  def intToValue(int: Int) = if (int == 0) false else true

  def valueToInt(value: Value) = if (value) 1 else 0

  def one = true

  def zero = false

  override val dimensions = Dimensions(Array(0 until 2))
}

class DoubleDom extends AtomicDom {
  dom =>
  type Value = Double
  type Var = DomVar
  type Marginals = Double

  def toValue(setting: Setting, offsets: Offsets = Offsets()) =
    setting.cont(offsets.contOff)


  def toMarginals(msg: Msg, offsets: Offsets) = {
    msg.cont(offsets.contOff).mean
  }

  def copyValue(value: Value, setting: Setting, offsets: Offsets = Offsets()) =
    setting.cont(offsets.contOff) = value


  def copyMarginals(marginals: Marginals, msgs: Msg, offsets: Offsets) = {
    msgs.cont(offsets.contOff) = new ContMsg(marginals)
  }

  def fillZeroMsg(target: Msg, offsets: Offsets) = {
    target.cont(offsets.contOff) = new ContMsg(0.0)
  }

  val lengths = Offsets(0, 1, 0, 0)

  def variable(name: String, offsets: Offsets = Offsets(), owner: term.Var[Dom]): Var = StaticDoubleVar(name, owner, offsets.contOff)

  def dynamic(name: => String, offsets: => Offsets, owner: term.Var[Dom]): Var = new BaseVar(name, owner) with DomVar {
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

      def argmax(observed: Array[Setting], msgs: Array[Msg], result: Array[Setting]) = {
        result(0).cont(0) = if (contained) Double.PositiveInfinity else observed(0).cont(0)
      }
    }
  }

  case class StaticDoubleVar(name: String, owner: term.Var[Dom], offset: Int) extends Var with DomTerm {
    override val ranges = super.ranges
  }

}

trait GenericDiscreteDom[T] extends AtomicDom {

  dom =>
  type Value = T
  type Var = DomVar
  type Marginals = Map[T,Double]


  def start = valueToInt(zero)
  def end = start + domainSize

  def intToValue(int: Int): Value

  def valueToInt(value: Value): Int

  def domainSize: Int

  def toValue(setting: Setting, offsets: Offsets = Offsets()) =
    intToValue(setting.disc(offsets.discOff))


  def domainValues = for (i <- start until end) yield intToValue(i)

  def toMarginals(msg: Msg, offsets: Offsets) = {
    val pairs = for (i <- start until end) yield intToValue(i) -> msg.disc(offsets.discOff).msg(i - start)
    pairs.toMap
  }

  def copyValue(value: Value, setting: Setting, offsets: Offsets = Offsets()) =
    setting.disc(offsets.discOff) = valueToInt(value)


  def copyMarginals(marginals: Marginals, msgs: Msg, offsets: Offsets) = {
    val start = valueToInt(zero)
    val end = start + domainSize
    val margs = domainValues map (v => marginals(v))
    msgs.disc(offsets.discOff) = new DiscMsg(margs.toArray)
  }

  def fillZeroMsg(target: Msg, offsets: Offsets) = {
    target.disc(offsets.discOff) = new DiscMsg(domainSize)
  }

  val lengths = Offsets(1, 0, 0, 0)

  def variable(name: String, offsets: Offsets = Offsets(), owner: term.Var[Dom]) = StaticDiscVar(name, owner, offsets.discOff)

  def dynamic(name: => String, offsets: => Offsets, owner: term.Var[Dom]) = new BaseVar(name, owner) with DomVar {
    def offset = offsets.discOff
  }

  trait DomVar extends Atom[dom.type] with super.DomVar {
    def ranges = Ranges(Offsets(offset, 0, 0, 0), Offsets(offset + 1, 0, 0, 0))
  }

  case class StaticDiscVar(name: String, owner: term.Var[Dom], offset: Int) extends DomVar {
    override val ranges = super.ranges
  }


}

class DiscreteDom[T](val values: IndexedSeq[T]) extends GenericDiscreteDom[T] {
  val index = values.zipWithIndex.toMap

  def intToValue(int: Int) = values(int)

  def valueToInt(value: Value) = index(value)

  def domainSize = values.size

  def one = values.last

  def zero = values.head

  override val dimensions = Dimensions(Array(values.indices))
}

case class RangeDom(values: Range) extends GenericDiscreteDom[Int] {
  def intToValue(int: Int) = int

  def valueToInt(value: Value) = value

  def domainSize = values.size

  def one = values.last

  def zero = values.head

  override val dimensions = Dimensions(Array(values))

  trait SampleTerm extends Term {

    abstract class Evaluator(in: Settings) extends AbstractEvaluator2(in) {
      val output = createSetting()
      private var currentExecution: Execution = null
      private var currentValue: Int = -1

      def nextValue(): Int

      def eval()(implicit execution: Execution) = {
        currentValue = nextValue()
        output.disc(0) = currentValue
      }

    }

    def vars = Seq.empty

    def atomsIterator = ???

    def evaluator() = ???

    def differentiator(wrt: Seq[term.Var[Dom]]) = ???
  }

  def uniform(implicit random: Random) = new SampleUniform()

  def shuffled(implicit random: Random) = new SampleShuffled()

  def sequential = SampleSequential

  case class SampleUniform(implicit random: Random) extends SampleTerm {
    override def evaluatorImpl(in: Settings) = new Evaluator(in) {

      def nextValue() = random.nextInt(domainSize) + values.start

    }
  }

  case class SampleShuffled(implicit random: Random) extends SampleTerm {
    val indexed = values.toIndexedSeq

    override def evaluatorImpl(in: Settings) = new Evaluator(in) {
      private var shuffled = random.shuffle(indexed).toIterator

      def nextValue() = {
        if (!shuffled.hasNext) {
          shuffled = random.shuffle(indexed).toIterator
        }
        shuffled.next()
      }
    }
  }

  case object SampleSequential extends SampleTerm {
    override def evaluatorImpl(in: Settings) = new Evaluator(in) {
      var iterator = values.iterator

      def nextValue() = {
        if (!iterator.hasNext) {
          iterator = values.iterator
        }
        iterator.next()
      }
    }
  }


}




