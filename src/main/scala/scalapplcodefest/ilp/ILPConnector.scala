package scalapplcodefest.ILP

import scala.collection.mutable

/**
 * This trait has to be implemented by every ILP implementation.
 *
 * @tparam T
 */
trait ILPConnector [T]{
  /**
   * Adds an ILP constraint.
   *
   * @param constraint
   * @return
   */
  def addConstraint(constraint: LPConstraint[T]):ILPConnector[T]

  /**
   * Adds a variable and its objective value to the ILP.
   *
   * @param variable
   * @return
   */
  def addVar(variable : Var[T]):ILPConnector[T]

  /**
   * Solves the ILP.
   *
   * @return
   */
  def solve():mutable.HashMap[T,Double]
}

/**
 * An ILP Variable.
 *
 * @param name Represents the variable. Two variables are equal if their names are equal.
 * @param value Value with which the variable is multiplied in the objective (e.g. value = 0.5 in 'max 0.5 x')
 * @param range Defines if the variable is boolean, integer, or any double range.
 * @tparam T
 */
class Var[T] (val name:T, val value: Double = 0, val range: Type.Value = Type.int){

  def canEqual(other: Any): Boolean = other.isInstanceOf[Var[T]]

  override def equals(other: Any): Boolean = other match {
    case that: Var[T] =>
      (that canEqual this) &&
        name == that.name
    case _ => false
  }
  override def hashCode(): Int = {
    val state = Seq(name)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString():String = {
    name.toString()
  }
}

/**
 * Defines an ILP constraint of the form:
 *
 * lhs op rhs
 *
 * Example
 * 1 x1 + 2 x2 <= 3
 *
 * equals the call:
 *
 * new LPConstraint(List((1,"x1"), (2,"x2")).Operator.leq, 3)
 *
 * @param lhs
 * @param op
 * @param rhs
 * @tparam T
 */
case class LPConstraint[T] (lhs: List[(Double,T)],op: Operator.Value = Operator.leq,rhs:Double)

/**
 * Operators of a linear inequality constraint.
 */
object Operator extends Enumeration {
  val geq = Value("geq")
  val leq = Value("leq")
  val <= = Value("leq")
  val >= = Value("geq")
}

/**
 * Types of an ILP variable.
 */
object Type extends Enumeration {
  val int = Value("int")
  val binary = Value("binary")
  val real = Value("real")
}