package scalapplcodefest.ILP

/**
 * Created by Jan on 02.01.14.
 */
trait ILPConnector [+T]{
  def addConstraint(constraint: LPConstraint[T]):this.type

  def addVar(variable : Var[T]):this.type

  def solve():Map[T,Double]

}

case class Var[T] (name:T){
  var range = Type.int
  var value:Double = 0
}

case class LPConstraint[T] (lhs: List[(Double,T)],op: Operator.type = Operator.leq,rhs:Double)

object Operator extends Enumeration {
  val geq = Value("geq")
  val leq = Value("leq")
  val <= = Value("leq")
  val >= = Value("geq")
}
object Type extends Enumeration {
  val int = Value("int")
  val binary = Value("binary")
  val real = Value("real")
}