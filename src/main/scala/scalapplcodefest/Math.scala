package scalapplcodefest

/**
 * @author Sebastian Riedel
 */
object Math {

  trait BinaryOperatorSameDomain[T] extends Fun[(T,T),T] {
    def dom:Set[T]
    def superDomain = CartesianProduct2(dom,dom)
    def targetSet = dom
    def isDefinedAt(x: (T, T)) = true
  }

  trait BinaryOperator[T,R] extends Fun[(T,T),R] {
    def dom:Set[T]
    def superDomain = CartesianProduct2(dom,dom)
    def isDefinedAt(x: (T, T)) = true
  }

  object Dot extends BinaryOperator[Vec,Double] {
    def targetSet = Doubles
    def apply(v1: (Vec, Vec)) = v1._1 dot v1._2
    def dom = Vecs
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

  case object And extends BinaryOperatorSameDomain[Boolean] {
    def apply(v1: (Boolean, Boolean)) = v1._1 && v1._2
    def dom = Bools
  }

  case object Or extends BinaryOperatorSameDomain[Boolean] {
    def apply(v1: (Boolean, Boolean)) = v1._1 || v1._2
    def dom = Bools
  }

  object VecAdd extends BinaryOperatorSameDomain[Vec] {
    def apply(v1: (Vec, Vec)) = v1._1 + v1._2
    def dom = Vecs
  }

  case class UnitVec(index:Term[Int],value:Term[Double]) extends Term[Vec] {
    def eval(state: State) = for (i <- index.eval(state);v <- value.eval(state)) yield new UnitVector(i,v)
    def variables = SetUtil.SetUnion(List(index.variables,value.variables))
    def domain[C >: Vec] = Constant(Vecs).asInstanceOf[Term[Set[C]]]
    def default = new UnitVector(index.default,value.default)
  }

}
