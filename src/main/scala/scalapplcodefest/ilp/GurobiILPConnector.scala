package scalapplcodefest.ilp

import gurobi._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.util.Random
import scala.annotation.tailrec
import org.scalautils.{Good, Bad, Or}


/**
 * Created by Jan on 02.01.14.
 */
class GurobiILPConnector[T] extends ILPConnector[T]{
  private val env = new GRBEnv("gurobi.log")
  private val model = new GRBModel(env)
  private val varsToGrb = new mutable.HashMap[ILPVar[T],GRBVar]
  private val grbToVars = new mutable.HashMap[GRBVar,ILPVar[T]]

  //val useLazy = true

  private val constraints = new ListBuffer[ILPConstraint[T]]
  private val cpiConstraints = new ListBuffer[ILPConstraint[T]]



  def close(){
    model.dispose()
    env.dispose()
  }

  /**
   * Adds an ILP Constraint. All variables of the constraint must be added before with the "addVariable" function.
   *
   * @param constraint
   * @return
   */
  override def addConstraint(constraint: ILPConstraint[T]) = {
    require((for{
      (_,v) <- constraint.lhs
    } yield varsToGrb contains v) reduceLeft (_ & _), "All variables of constraint "+constraint+ " must be added before.")
    if(constraint.cpi) cpiConstraints += constraint else constraints += constraint
    this
  }

  /**
   * Adds a variable. It is not allowed to add a variable twice.
   *
   * @param variable
   * @return
   */
  override def addVar(variable : ILPVar[T]) = {
    require(varsToGrb.get(variable)==None, "Variable "+variable.name+" has been added before. Only add variables once.")
    val range = variable.range match {
      case ILPType.binary => GRB.BINARY
      case ILPType.int => GRB.INTEGER
      case ILPType.real => GRB.CONTINUOUS
    }
    val grbVar = model.addVar(0.0, GRB.INFINITY, variable.value, range, "")
    varsToGrb.put(variable, grbVar)
    grbToVars.put(grbVar, variable)
    this
  }

  private def constructConstraint(constraint: ILPConstraint[T]): (GRBLinExpr, Char, Double)={
    val expr = new GRBLinExpr()
    for{
      (value, variable) <- constraint.lhs
    } yield expr.addTerm(value, varsToGrb get variable get)
    val operator = constraint.op match{
      case ILPOperator.geq => GRB.GREATER_EQUAL
      case ILPOperator.leq => GRB.LESS_EQUAL
    }
    (expr, operator, constraint.rhs)
  }

  private def constrToILP(constraint: ILPConstraint[T]){
    val (expr, op, rhs) = constructConstraint(constraint)
    model.addConstr(expr, op, rhs, "")
  }

  /**
   * Solves the ILP.
   *
   * @return The solution of all the ILP variables.
   */
  override def solve(): scala.collection.Set[ILPVar[T]] Or ILPState.Value = {
    model.update()
    println("- add " + constraints.size + " constraints.")
    for{constraint <- constraints
       c = constraint
    } yield {
       constrToILP(c)
    }

    //maximizing problem
    model.set(GRB.IntAttr.ModelSense, -1)
//  debug output.
    //model.update()
    //model.write("model.lp")
    val state = optimizeModel()
    if(state == ILPState.optimal){
      model.getVars().foreach { v => grbToVars(v).result= v.get(GRB.DoubleAttr.X) }
      // run cutting plane inference. If no cpiConstraints are in the model, it just returns the result.
      runCPI(Double.MaxValue, 1, cpiConstraints)
    }else{
      Bad(state)
    }
  }

  private def optimizeModel(): ILPState.Value = {
    model.optimize()
    model.get(GRB.IntAttr.Status) match  {
      case GRB.OPTIMAL => ILPState.optimal
      case GRB.INFEASIBLE => ILPState.infeasible
      case GRB.UNBOUNDED => ILPState.unbounded
    }
  }

  /**
   * Recursively runs cutting plane inference until either no more cpi constraint remain or the objective is not changing any more.
   *
   * @param obj current objective value
   * @param iteration current ilp iteration (just for printout information)
   * @param cpiConstraints current ILP Constraint canidates (will get fewer with each iteration)
   * @return
   */
  @tailrec
  private def runCPI(obj: Double, iteration: Integer, cpiConstraints: ListBuffer[ILPConstraint[T]]): scala.collection.Set[ILPVar[T]] Or ILPState.Value = {
    val currentObj =model.get(GRB.DoubleAttr.ObjVal)
    if(currentObj<obj && cpiConstraints.size>0){
      println("- CPI Iteration " + iteration)
      // assigning old objective
      // adding violated constraints
      val reducedCpiConstraints = cpiConstraints.filter(c => {
        if(c.isViolated()){
          constrToILP(c)
          false
        }else true
      })
      println("- "+ cpiConstraints.size+" violated Constraints remaining ")
      //model.update()
      //model.write("model"+iteration+".lp")
      // solving again
      val state = optimizeModel()
      if(state == ILPState.optimal){
        model.getVars().foreach { v => grbToVars(v).result= v.get(GRB.DoubleAttr.X) }
        runCPI(currentObj, iteration+1, reducedCpiConstraints)
      } else {
        Bad(state)
      }
    }else{
      Good(varsToGrb.keySet)
    }
  }
}

object GurobiILPConnector{
  def main(args: Array[String]) {
    val gurobi = new GurobiILPConnector[String]
    val v1 = new ILPVar("hello",  1, ILPType.binary)
    val v2 = new ILPVar("hello2", 0.9, ILPType.binary)
    gurobi.addVar(v1).addVar(v2)
      .addConstraint(new ILPConstraint[String](List((1,v1),(1,v2)),ILPOperator.leq,1,true))
      .addConstraint(new ILPConstraint[String](List((1,v1),(1,v1)),ILPOperator.leq,1,true))
      .addConstraint(new ILPConstraint[String](List((1,v2),(1,v2)),ILPOperator.leq,2,true))
    val res = gurobi.solve().get
    for{v <- res} yield println(v.name + " = " + v.result)

    //val r = new Random(1234)
/*    val r = new Random(12345)

    val startCPI = System.currentTimeMillis()
    val vars = for{
      i <- (1 to 800)
      v = new ILPVar("v" + i, (r.nextDouble()-0.5), ILPType.binary)
    } yield v

    vars foreach {v => gurobi.addVar(v)}

    vars foreach {v1 => vars foreach {v2 => if(r.nextDouble()>0.5)
      gurobi.addConstraint(new ILPConstraint[String](List((1,v1),(1,v2)),ILPOperator.leq,1,true))}}
    val res = gurobi.solve()
    println("took " + (System.currentTimeMillis()-startCPI))
*/
  }
}
