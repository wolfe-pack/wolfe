package scalapplcodefest

/**
 * @author Sebastian Riedel
 */
object Math {

  trait BinaryOperator[T] extends Fun[(T,T),T] {
    def dom:Set[T]
    def superDomain = CartesianProduct2(dom,dom)
    def targetSet = dom
    def isDefinedAt(x: (T, T)) = true
  }

  object IntAdd extends BinaryOperator[Int] {
    def apply(v1: (Int, Int)) = v1._1 + v1._2
    def dom = Ints
  }

  object DoubleAdd extends BinaryOperator[Double] {
    def apply(v1: (Double, Double)) = v1._1 + v1._2
    def dom = Doubles
  }

  object And extends BinaryOperator[Boolean] {
    def apply(v1: (Boolean, Boolean)) = v1._1 && v1._2
    def dom = Bools
  }

  object Or extends BinaryOperator[Boolean] {
    def apply(v1: (Boolean, Boolean)) = v1._1 || v1._2
    def dom = Bools
  }

  object VecAdd extends BinaryOperator[Vec] {
    def apply(v1: (Vec, Vec)) = v1._1 + v1._2
    def dom = Vecs
  }

  case class UnitVec(index:Term[Int],value:Term[Double]) extends Term[Vec] {
    def eval(state: State) = for (i <- index.eval(state); v <- value.eval(state)) yield new UnitVector(i,v)
    def variables = SetUtil.SetUnion(List(index.variables,value.variables))
    def domain[C >: Vec] = Constant(Vecs).asInstanceOf[Term[Set[C]]]
    def default = new UnitVector(index.default,value.default)
  }

}
