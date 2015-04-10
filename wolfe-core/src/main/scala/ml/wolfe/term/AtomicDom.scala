package ml.wolfe.term

import cc.factorie.la.{GrowableDenseTensor1, DenseTensor1, DenseTensor2}
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





class MatrixDom(val dim1: Int, dim2: Int) extends AtomicDom {
  dom =>
  type Value = Mat
  type Var = DomVar
  type Marginals = Mat

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

  def Variable(name: String): DomVar = new StaticMatrixVar(name)

  def one = new DenseTensor2(dim1, dim2, 1.0)

  def zero = new DenseTensor2(dim1, dim2, 0.0)

  case class StaticMatrixVar(name: String) extends DomVar {
  }

  trait DomVar extends DomTerm with super.DomVar {
  }

}

class BooleanDom extends GenericDiscreteDom[Boolean] {

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

  def Variable(name: String): Var = StaticDoubleVar(name)

  def one = 1.0

  def zero = 0.0

  case class StaticDoubleVar(name: String) extends Var with DomTerm

  override def toString = "Doubles"
}



class DiscreteDom[T](raw: IndexedSeq[T]) extends GenericDiscreteDom[T] {

  val values = raw.distinct

  val index = values.zipWithIndex.toMap

  def intToValue(int: Int) = values(int)

  def valueToInt(value: Value) = index(value)

  def one = values.last

  def zero = values.head

  override val dimensions = Dimensions(Array(values.indices))

  override def toString = s"""Discretes(${values.mkString(",")})"""

  def withOOV(oov:T) = new DiscreteOOVDom(raw, oov)
}

class DiscreteOOVDom[T](raw: IndexedSeq[T], val oov:T) extends GenericDiscreteDom[T] {

  val values = (raw :+ oov).distinct

  val index = values.zipWithIndex.toMap withDefaultValue -1

  def intToValue(int: Int) = if (int == -1) oov else values(int)

  def valueToInt(value: Value) = index(value)

  def one = values.last

  def zero = values.head

  override val dimensions = Dimensions(Array(values.indices))

  override def toString = s"""Discretes(${values.mkString(",")})"""
}



case class RangeDom(values: Range) extends GenericDiscreteDom[Int] {

  def +(that:RangeDom) = RangeDom((this.values.start + that.values.start) until (this.values.last + that.values.last) + 1)

  def -(that:RangeDom) = RangeDom((this.values.start - that.values.start) until (this.values.last - that.values.last) + 1)

  def intToValue(int: Int) = int

  def valueToInt(value: Value) = value

  def one = values.last

  def zero = values.start

  override val dimensions = Dimensions(Array(values))

  trait SampleTerm extends Term {

    def isStatic = false

    abstract class Evaluator(in: Settings) extends AbstractEvaluator(in) {
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





