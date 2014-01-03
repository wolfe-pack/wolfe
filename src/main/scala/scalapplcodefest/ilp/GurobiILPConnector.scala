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


  def addConstraint(constraint: LPConstraint[T]): GurobiILPConnector[T] = {
    require((for{(_,v) <- constraint.lhs} yield varsToGrb contains v) reduceLeft (_ & _), "All variables of constraint "+constraint+ " must be added before.")
    constraints += constraint
    this
  }

  def addVar(variable : Var[T]): GurobiILPConnector[T] = {
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

  def solve(): mutable.HashMap[T, Double] = {
    model.update()
    for{constraint <- constraints
       (lhs:List[(Double,T)],op:Operator.type,rhs:Double) = constraint
    } yield {
      var expr = new GRBLinExpr();
      for{
        (value, variable) <- lhs
      } yield expr.addTerm(value, varsToGrb get variable get)
      val operator = op match{
        case Operator.geq => GRB.GREATER_EQUAL
        case Operator.leq => GRB.LESS_EQUAL
      }
      model.addConstr(expr, operator, rhs, "");
    }

    model.write("model.lp")
    model.optimize()
    val map = new mutable.HashMap[T, Double]
    model.getVars().foreach { v => map.put(grbToVars(v), v.get(GRB.DoubleAttr.X)) }
    map
  }
}
