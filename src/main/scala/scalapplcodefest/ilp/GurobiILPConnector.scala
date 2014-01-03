package scalapplcodefest.ILP

import gurobi._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable


/**
 * Created by Jan on 02.01.14.
 */
class GurobiILPConnector[T] extends ILPConnector[T]{
  private val env = new GRBEnv("gurobi.log")
  private var model = new GRBModel(env)
  private val varsToGrb = new mutable.HashMap[T,GRBVar]
  private val grbToVars = new mutable.HashMap[GRBVar,T]

  private val constraints = new ListBuffer[LPConstraint[T]]

  /**
   * Adds an ILP Constraint. All variables T of the constraint must be added before with the "addVariable" function.
   *
   * @param constraint
   * @return
   */
  override def addConstraint(constraint: LPConstraint[T]) = {
    require((for{(_,v) <- constraint.lhs} yield varsToGrb contains v) reduceLeft (_ & _), "All variables of constraint "+constraint+ " must be added before.")
    constraints += constraint
    this
  }

  /**
   * Adds a variable. It is not allowed to add a variable twice.
   *
   * @param variable
   * @return
   */
  override def addVar(variable : Var[T]) = {
    require(varsToGrb.get(variable.name)==None, "Variable "+variable.name+" has been added before. Only add variables once.")
    val range = variable.range match {
      case Type.binary => GRB.BINARY
      case Type.int => GRB.INTEGER
      case Type.real => GRB.CONTINUOUS
    }
    val grbVar = model.addVar(0.0, GRB.INFINITY, variable.value, range, "")
    varsToGrb.put(variable.name, grbVar)
    grbToVars.put(grbVar, variable.name)
    this
  }

  /**
   * Solves the ILP.
   *
   * @return The solution of all the ILP variables.
   */
  override def solve(): mutable.HashMap[T, Double] = {
    model.update()
    for{constraint <- constraints
       c = constraint
    } yield {
      var expr = new GRBLinExpr()
      for{
        (value, variable) <- c.lhs
      } yield expr.addTerm(value, varsToGrb get variable get)
      val operator = c.op match{
        case Operator.geq => GRB.GREATER_EQUAL
        case Operator.leq => GRB.LESS_EQUAL
      }
      model.addConstr(expr, operator, c.rhs, "")
    }

    model.update()
    //maximizing problem
    model.set(GRB.IntAttr.ModelSense, -1)

    model.write("model.lp")
    model.optimize()
    val map = new mutable.HashMap[T, Double]
    model.getVars().foreach { v => map.put(grbToVars(v), v.get(GRB.DoubleAttr.X)) }
    map
  }
}

object GurobiILPConnector{
  def main(args: Array[String]) {
    val gurobi = new GurobiILPConnector[String]
    gurobi.addVar(new Var("hello",  1, Type.binary)).addVar(new Var("hello2", 0.9, Type.binary)).addConstraint(new LPConstraint(List((1,"hello"),(1,"hello2")),Operator.leq,1))
    println(gurobi.solve())
  }
}