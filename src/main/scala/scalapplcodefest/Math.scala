package scalapplcodefest

import cc.factorie.la.SingletonTensor1

/**
 * Math-related function objects that can serve as terms when wrapped with a constant.
 * @author Sebastian Riedel
 */
object Math {

  trait BinaryOperatorSameDomain[T] extends Fun[(T, T), T] {
    def dom: Set[T]
    def funCandidateDom = CartesianProduct2(dom, dom)
    def funRange = dom
    def isDefinedAt(x: (T, T)) = true
  }

  trait BinaryOperator[T, R] extends Fun[(T, T), R] {
    def dom: Set[T]
    def funCandidateDom = CartesianProduct2(dom, dom)
    def isDefinedAt(x: (T, T)) = true
  }

  object Dot extends BinaryOperator[Vector, Double] {
    def funRange = Doubles
    def apply(v1: (Vector, Vector)) = v1._1 dot v1._2
    def dom = Vectors
  }

  case object IntAdd extends BinaryOperatorSameDomain[Int] {
    def apply(v1: (Int, Int)) = v1._1 + v1._2
    def dom = Ints
  }

  case object IntMinus extends BinaryOperatorSameDomain[Int] {
    def apply(v1: (Int, Int)) = v1._1 - v1._2
    def dom = Ints
  }

  case object DoubleAdd extends BinaryOperatorSameDomain[Double] {
    def apply(v1: (Double, Double)) = v1._1 + v1._2
    def dom = Doubles
  }

  case object DoubleMultiply extends BinaryOperatorSameDomain[Double] {
    def apply(v1: (Double, Double)) = v1._1 * v1._2
    def dom = Doubles
  }

  trait BinaryBoolOperator extends BinaryOperatorSameDomain[Boolean] {
    def dom = Bools

  }

  case object And extends BinaryBoolOperator {
    def apply(v1: (Boolean, Boolean)) = v1._1 && v1._2
  }

  case object Or extends BinaryBoolOperator {
    def apply(v1: (Boolean, Boolean)) = v1._1 || v1._2
  }
  case object Implies extends BinaryBoolOperator {
    def apply(v1: (Boolean, Boolean)) = !v1._1 || v1._2
  }

  case object Neg extends Fun[Boolean,Boolean] {
    def funCandidateDom = Bools
    override def funDom = Bools
    def funRange = Bools
    def isDefinedAt(x: Boolean) = true
    def apply(v1:Boolean) = !v1
  }

  case object VecAdd extends BinaryOperatorSameDomain[Vector] {
    def apply(pair: (Vector, Vector)) = {
      pair match {
        case (s1:SingletonVector, s2:SingletonVector) =>
          val result = new SparseVector(2) // what should the dimension be?
          result += s1
          result += s2
          result
        case (singleton:SingletonVector,other) => other + singleton
        case (other, singleton:SingletonVector) => other + singleton
        case (v1,v2) => v1 + v2
      }
    }
    def dom = Vectors
  }

  case object Iverson extends Fun[Boolean, Double] {
    def funCandidateDom = Bools
    override def funDom = Bools
    def funRange = Doubles
    def isDefinedAt(x: Boolean) = true
    def apply(x: Boolean) = if (x) 1.0 else 0.0
  }

  case class UnitVec(index: Term[Int], value: Term[Double]) extends Term[Vector] {
    def eval(state: State) = for (i <- index.eval(state); v <- value.eval(state)) yield
      new SingletonTensor1(1, i, v)
    def variables = SetUtil.SetUnion(List(index.variables, value.variables))
    def domain[C >: Vector] = Constant(Vectors).asInstanceOf[Term[Set[C]]]
    def default = new SingletonTensor1(1,index.default, value.default)
  }

}
