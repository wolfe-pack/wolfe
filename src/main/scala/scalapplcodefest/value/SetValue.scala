package scalapplcodefest.value

import scala.language.existentials
import cc.factorie.la.{SingletonTensor1, ScalarTensor}
import scala.collection.SetProxy
import scalapplcodefest._
import scalapplcodefest.term._
import scalapplcodefest.term.Constant

/**
 * A set that performs lazy set union and set minus
 * @author Sebastian Riedel
 */
trait SetValue[T] extends Set[T] {
  def +(elem: T): Set[T] = SetUtil.SetUnion(List(this, Set(elem)))
  def -(elem: T): Set[T] = SetUtil.SetMinus(this, Set(elem))
  def --(that: Set[T]): Set[T] = SetUtil.SetMinus(this, that)
  def ++(that: Set[T]): Set[T] = SetUtil.SetUnion(List(this, that))
}

/**
 * Helper to build SetValue objects.
 */
case class SeqSet[T](elems: Seq[T]) extends SetValue[T] {
  val self = elems.toSet
  def contains(elem: T) = self(elem)
  def iterator = elems.iterator
}

/**
 * Turns a set with specific type to a GENeric set over Any objects.
 * @param set the set to convert to a generic set.
 * @tparam T type of elements in original set.
 */
case class Gen[T](set: Set[T]) extends SetProxy[Any] {
  def self = set.asInstanceOf[Set[Any]]
}

/**
 * A Term that represents the reduce operation applied to a sequence of values.
 * @param op the binary operator used to reduce elements.
 * @param arguments the elements to be reduced.
 * @tparam T the type of elements to reduce.
 */
case class Reduce[T](op: Term[Fun[(T, T), T]], arguments: Term[Seq[T]]) extends Term[T] {
  val FunTerm(_, funRange) = op
  def eval(state: State) = for (f <- op.eval(state); set <- arguments.eval(state)) yield
    set.reduce((a1, a2) => f(a1 -> a2))
  def variables = SetUtil.SetUnion(List(op.variables, arguments.variables))
  def domain[C >: T] = funRange.asInstanceOf[Term[Set[C]]]
  def default = funRange.default.head
}

/**
 * A term that is evaluated to a range of integers.
 * @param from starting integer (included)
 * @param to end integer (excluded)
 */
case class RangeSet(from: Term[Int], to: Term[Int]) extends Term[Set[Int]] with Composite2[Int,Int,Set[Int]] {
  def eval(state: State) =
    for (f <- from.eval(state);
         t <- to.eval(state)) yield RangeSetValue(f, t)
  def variables = SetUtil.SetUnion(List(from.variables, to.variables))
  def default = RangeSetValue(from.default, to.default + 1)
  def domain[C >: Set[Int]] = Constant(new AllOfType[C])
  override def toString = s"($from ~~ $to)"
  def components = (from,to)
  def copy(t1: Term[Int], t2: Term[Int]) = RangeSet(t1,t2)
}

/**
 * Set-version of a [[scala.collection.immutable.Range]].
 * @param from start (included)
 * @param to end (excluded)
 */
case class RangeSetValue(from: Int, to: Int) extends SetValue[Int] {
  val range = Range(from, to)
  val rangeSet = range.toSet
  def contains(elem: Int) = rangeSet(elem)
  def iterator = range.iterator
}

trait AllObjects[T] extends SetValue[T] {
  def contains(elem: T) = true
  override def hashCode() = System.identityHashCode(this)
  override def equals(that: Any) = that match {
    case x: AnyRef => x eq this
    case _ => false
  }
  override def +(elem: T) = this


}

/**
 * Set of all integers.
 */
case object Ints extends AllObjects[Int] {
  def iterator = Util.tooLargeToIterate
  override def size = Util.tooLargeToCount
  override def head = 0

  case object Range extends BinaryOperatorSameDomain[Int,Set[Int]] {
    def dom = Ints
    def funRange = new AllOfType[Set[Int]]
    def apply(x:(Int,Int)) = RangeSetValue(x._1,x._2)
  }

  case object Add extends BinaryOperatorSameDomainAndRange[Int] {
    def apply(v1: (Int, Int)) = v1._1 + v1._2
    def dom = Ints
  }

  case object Minus extends BinaryOperatorSameDomainAndRange[Int] {
    def apply(v1: (Int, Int)) = v1._1 - v1._2
    def dom = Ints
  }

  case object ExactlyOne extends Operator[Seq[Boolean],Double] {
    def funCandidateDom = new AllOfType[Seq[Boolean]]
    def funRange = Doubles
    def apply(x: Seq[Boolean]) = if (x.count(identity) == 1) 0.0 else Double.NegativeInfinity
  }

  case object Divide extends Fun[(Int,Int),Int] {
    def isDefinedAt(x: (Int, Int)) = x._2 != 0
    def apply(x:(Int,Int)) = x._1 / x._2
    def funCandidateDom = CartesianProduct2(Ints,Ints)
    def funRange = Ints
  }

}

trait AllObjectsLarge[T] extends AllObjects[T] {
  def iterator = Util.tooLargeToIterate
  override def toString() = getClass.getSimpleName
}

/**
 * Set of all vectors.
 */
case object Vectors extends AllObjectsLarge[Vector] {
  override def size = Util.tooLargeToCount
  override def head = new ScalarTensor(0.0)

  object Dot extends BinaryOperatorSameDomain[Vector, Double] {
    def funRange = Doubles
    def apply(v1: (Vector, Vector)) = v1._1 dot v1._2
    def dom = Vectors
  }


  case object VecAdd extends BinaryOperatorSameDomainAndRange[Vector] {
    def apply(pair: (Vector, Vector)) = {
      pair match {
        case (s1: SingletonVector, s2: SingletonVector) =>
          val result = new SparseVector(2) // what should the dimension be?
          result += s1
          result += s2
          result
        case (singleton: SingletonVector, other) => other + singleton
        case (other, singleton: SingletonVector) => other + singleton
        case (v1, v2) => v1 + v2
      }
    }
    def dom = Vectors
  }

  case object VecMinus extends BinaryOperatorSameDomainAndRange[Vector] {
    def apply(pair: (Vector, Vector)) = {
      pair match {
        case (s1: SingletonVector, s2: SingletonVector) =>
          val result = new SparseVector(2) // what should the dimension be?
          result += s1
          result -= s2
          result
        case (singleton: SingletonVector, other) =>singleton - other
        case (other, singleton: SingletonVector) => singleton - other
        case (v1, v2) => v1 - v2
      }
    }
    def dom = Vectors
  }

  case object UnitVector extends Operator[(Int,Double),Vector] {
    def funCandidateDom = CartesianProduct2(Ints,Doubles)
    def funRange = Vectors
    def apply(indexValue:(Int,Double)) = new SingletonTensor1(1, indexValue._1, indexValue._2)
  }


}


/**
 * All Boolean objects.
 */
case object Bools extends AllObjects[Boolean] {
  def iterator = Iterator(false, true)
  override def head = false

  trait BinaryBoolOperator extends BinaryOperatorSameDomainAndRange[Boolean] {def dom = Bools}

  case object And extends BinaryBoolOperator {def apply(v1: (Boolean, Boolean)) = v1._1 && v1._2}
  case object Or extends BinaryBoolOperator {def apply(v1: (Boolean, Boolean)) = v1._1 || v1._2}
  case object Implies extends BinaryBoolOperator {def apply(v1: (Boolean, Boolean)) = !v1._1 || v1._2}
  case object Equiv extends BinaryBoolOperator {def apply(v1: (Boolean, Boolean)) = v1._1 == v1._2}


  case object Neg extends Operator[Boolean, Boolean] {
    def funCandidateDom = Bools
    override def funDom = Bools
    def funRange = Bools
    def apply(v1: Boolean) = !v1
  }

  case object Iverson extends Operator[Boolean, Double] {
    def funCandidateDom = Bools
    override def funDom = Bools
    def funRange = Doubles
    def apply(x: Boolean) = if (x) 1.0 else 0.0
  }


}

/**
 * All String objects.
 */
case object Strings extends AllObjectsLarge[String] {
  override def head = ""
}

/**
 * All Double objects.
 */
case object Doubles extends AllObjectsLarge[Double] {
  override def head = 0.0

  case object Add extends BinaryOperatorSameDomainAndRange[Double] {
    def apply(v1: (Double, Double)) = v1._1 + v1._2
    def dom = Doubles
  }

  case object Minus extends BinaryOperatorSameDomainAndRange[Double] {
    def apply(v1: (Double, Double)) = v1._1 - v1._2
    def dom = Doubles
  }
  case object Times extends BinaryOperatorSameDomainAndRange[Double] {
    def apply(v1: (Double, Double)) = v1._1 * v1._2
    def dom = Doubles
  }

  case object Log extends Operator[Double,Double] {
    def funCandidateDom = Doubles //todo: non-negative
    def funRange = Doubles
    def apply(x: Double) = math.log(x)
  }
}

class AllOfType[T] extends AllObjectsLarge[T] {
  val tag = manifest
  override def equals(that: Any) = that match {
    case a:AllOfType[_] => true  //todo: Waargh
    case _ => false
  }
}

case object All extends AllObjectsLarge[Any]

case object AllRefs extends AllObjectsLarge[AnyRef]



