package scalapplcodefest.value

import cc.factorie.la.SingletonTensor1
import cc.factorie.maths.ArrayOps
import java.util
import scalapplcodefest.term._
import scalapplcodefest._
import scalapplcodefest.Vector
import scalapplcodefest.term.CartesianProduct2

/**
 * Math-related function objects.
 *
 * @author Sebastian Riedel
 */
object Math {

  object Dot extends BinaryOperatorSameDomain[Vector, Double] {
    def funRange = Doubles
    def apply(v1: (Vector, Vector)) = v1._1 dot v1._2
    def dom = Vectors
  }

  case object IntAdd extends BinaryOperatorSameDomainAndRange[Int] {
    def apply(v1: (Int, Int)) = v1._1 + v1._2
    def dom = Ints
  }

  case object IntMinus extends BinaryOperatorSameDomainAndRange[Int] {
    def apply(v1: (Int, Int)) = v1._1 - v1._2
    def dom = Ints
  }

  case object DoubleAdd extends BinaryOperatorSameDomainAndRange[Double] {
    def apply(v1: (Double, Double)) = v1._1 + v1._2
    def dom = Doubles
  }

  case object DoubleAndVectorAdd extends BinaryOperatorSameDomainAndRange[ValueAndGradient] {
    def apply(x: (ValueAndGradient, ValueAndGradient)) = x match {case ((v1,g1),(v2,g2)) => (v1 + v2, g1 + g2)}
    def dom = CartesianProduct2(Doubles,Vectors)
  }

  case object DoubleAndVectorMinus extends BinaryOperatorSameDomainAndRange[ValueAndGradient] {
    def apply(x: (ValueAndGradient, ValueAndGradient)) = x match {case ((v1,g1),(v2,g2)) => (v1 - v2, g1 - g2)}
    def dom = CartesianProduct2(Doubles,Vectors)
  }



  case object DoubleMinus extends BinaryOperatorSameDomainAndRange[Double] {
    def apply(v1: (Double, Double)) = v1._1 - v1._2
    def dom = Doubles
  }


  case object ExactlyOne extends Operator[Seq[Boolean],Double] {
    def funCandidateDom = new AllOfType[Seq[Boolean]]
    def funRange = Doubles
    def apply(x: Seq[Boolean]) = if (x.count(identity) == 1) 0.0 else Double.NegativeInfinity
  }

  case object DoubleMultiply extends BinaryOperatorSameDomainAndRange[Double] {
    def apply(v1: (Double, Double)) = v1._1 * v1._2
    def dom = Doubles
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


  case object Log extends Operator[Double,Double] {
    def funCandidateDom = Doubles //todo: non-negative
    def funRange = Doubles
    def apply(x: Double) = math.log(x)
  }

  case object Iverson extends Operator[Boolean, Double] {
    def funCandidateDom = Bools
    override def funDom = Bools
    def funRange = Doubles
    def apply(x: Boolean) = if (x) 1.0 else 0.0
  }

  case class UnitVec(index: Term[Int], value: Term[Double]) extends Term[Vector] {
    def eval(state: State) = for (i <- index.eval(state); v <- value.eval(state)) yield
      new SingletonTensor1(1, i, v)
    def variables = SetUtil.SetUnion(List(index.variables, value.variables))
    def domain[C >: Vector] = Constant(Vectors).asInstanceOf[Term[Set[C]]]
    def default = new SingletonTensor1(1, index.default, value.default)
    override def toString = s"$value * e_($index)"
  }

}

object MoreArrayOps extends ArrayOps {
  def maxValue(s:A): Double = { var result = s(0); var i = 0; while (i < s.length) { if (s(i) > result) result = s(i); i += 1 }; result }
  def maxNormalize(s:A) { val norm = maxValue(s); this -= (s,norm)}
  def fill(s:A,v:Double) { util.Arrays.fill(s,v)}
}

/**
 * Logic related function objects.
 */
object Logic {

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


}