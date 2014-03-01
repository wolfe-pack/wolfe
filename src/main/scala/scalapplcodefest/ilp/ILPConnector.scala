package scalapplcodefest.ilp

import org.scalautils.Or

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
  def addConstraint(constraint: ILPConstraint[T]):ILPConnector[T]

  /**
   * Adds a variable and its objective value to the ILP.
   *
   * @param variable
   * @return
   */
  def addVar(variable : ILPVar[T]):ILPConnector[T]

  /**
   * Solves the ILP.
   *
   * @return
   */
  def solve():scala.collection.Set[ILPVar[T]] Or ILPState.Value
}

/**
 * An ILP Variable.
 *
 * @param name Represents the variable. Two variables are equal if their names are equal.
 * @param value Value with which the variable is multiplied in the objective (e.g. value = 0.5 in 'max 0.5 x')
 * @param range Defines if the variable is boolean, integer, or any double range.
 * @tparam T
 */
class ILPVar[T] (val name:T, val value: Double = 0, val range: ILPType.Value = ILPType.int){

  var result:Double =0

  def canEqual(other: Any): Boolean = other.isInstanceOf[ILPVar[T]]

  override def equals(other: Any): Boolean = other match {
    case that: ILPVar[T] =>
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
 * @param cpi Defines if Cutting plane inference is used (=true) or if the constraint is added to the ilp immediately (=false)
 * @tparam T
 */
case class ILPConstraint[T] (lhs: List[(Double,ILPVar[T])],op: ILPOperator.Value = ILPOperator.leq,rhs:Double, cpi:Boolean = false){

  /**
   * Checks if a current constraint is violated (dependent on the actual solution stored in the node.
   * @return
   */
  def isViolated():Boolean = {
    val lhsSum = (for {
      (value, variable) <- lhs
    }yield value*variable.result) reduceLeft(_+_)
    if(ILPOperator.leq == op){
      //println(lhsSum+ " <= " + rhs + " : " + (lhsSum>rhs))
      lhsSum > rhs
    }else{
      //println(lhsSum+ " >= " + rhs + " : " + (lhsSum>rhs))
      lhsSum < rhs
    }
  }
}

/**
 * Operators of a linear inequality constraint.
 */
object ILPOperator extends Enumeration {
  val geq = Value("geq")
  val leq = Value("leq")
}

/**
 * Types of an ILP variable.
 */
object ILPType extends Enumeration {
  val int = Value("int")
  val binary = Value("binary")
  val real = Value("real")
}

object ILPState extends Enumeration {
  /**
   * The ILP has been solved correctly and the solution is within the given bound.
   */
  val optimal = Value("optimal")
  /**
   * One or more variables are unbounded (not restricted). This leads to an infinitive objective and no (or no useful) result.
   */
  val unbounded = Value("unbounded")
  /**
   * Model has constraints which lead to an unsolvable model (e.g. x>5 and x<2).
   */
  val infeasible = Value("infeasible")
}