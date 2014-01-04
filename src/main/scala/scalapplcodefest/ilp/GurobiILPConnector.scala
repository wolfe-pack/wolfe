package scalapplcodefest.ILP

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
  private val varsToGrb = new mutable.HashMap[Var[T],GRBVar]
  private val grbToVars = new mutable.HashMap[GRBVar,Var[T]]

  //val useLazy = true

  private val constraints = new ListBuffer[LPConstraint[T]]
  private val cpiConstraints = new ListBuffer[LPConstraint[T]]

  /**
   * Adds an ILP Constraint. All variables T of the constraint must be added before with the "addVariable" function.
   *
   * @param constraint
   * @return
   */
  override def addConstraint(constraint: LPConstraint[T]) = {
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
  override def addVar(variable : Var[T]) = {
    require(varsToGrb.get(variable)==None, "Variable "+variable.name+" has been added before. Only add variables once.")
    val range = variable.range match {
      case Type.binary => GRB.BINARY
      case Type.int => GRB.INTEGER
      case Type.real => GRB.CONTINUOUS
    }
    val grbVar = model.addVar(0.0, GRB.INFINITY, variable.value, range, "")
    varsToGrb.put(variable, grbVar)
    grbToVars.put(grbVar, variable)
    this
  }

  private def constructConstraint(constraint: LPConstraint[T]): (GRBLinExpr, Char, Double)={
    val expr = new GRBLinExpr()
    for{
      (value, variable) <- constraint.lhs
    } yield expr.addTerm(value, varsToGrb get variable get)
    val operator = constraint.op match{
      case Operator.geq => GRB.GREATER_EQUAL
      case Operator.leq => GRB.LESS_EQUAL
    }
    (expr, operator, constraint.rhs)
  }

  private def constrToILP(constraint: LPConstraint[T]){
    val (expr, op, rhs) = constructConstraint(constraint)
    model.addConstr(expr, op, rhs, "")
  }

  /**
   * Solves the ILP.
   *
   * @return The solution of all the ILP variables.
   */
  override def solve(): scala.collection.Set[Var[T]] Or ILPState.Value = {
    model.update()
    println("- add " + constraints.size + " number of constraints.")
    for{constraint <- constraints
       c = constraint
    } yield {
       constrToILP(c)
    }

    //maximizing problem
    model.set(GRB.IntAttr.ModelSense, -1)
    /*if(useLazy){
      // Must set LazyConstraints parameter when using lazy constraints
      model.getEnv().set(GRB.IntParam.LazyConstraints, 1);
      model.setCallback(new CPI(Double.MaxValue, 1, cpiConstraints));
    }*/
//  debug output.
//  model.update()
//  model.write("model.lp")
    val state = optimizeModel()
    if(state == ILPState.optimal){
      model.getVars().foreach { v => grbToVars(v).result= v.get(GRB.DoubleAttr.X) }
      // run cutting plane inference. If no cpiConstraints are in the model, it just returns the result.
      //if(!useLazy) {}
        runCPI(Double.MaxValue, 1, cpiConstraints)
      //}else{
      //  Good(varsToGrb.keySet)
      //}
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
  private def runCPI(obj: Double, iteration: Integer, cpiConstraints: ListBuffer[LPConstraint[T]]): scala.collection.Set[Var[T]] Or ILPState.Value = {
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
      //      model.update()
      //      model.write("model"+i+".lp")
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

  /*private class CPI(var obj: Double, var iteration: Integer, var cpiConstraintsInt: ListBuffer[LPConstraint[T]]) extends GRBCallback{

    /**
     * Checks if a current constraint is violated (dependent on the actual solution stored in the node.
     * @return
     */
    def isViolatedLazy(c: LPConstraint[T]):Boolean = {
      val lhsSum = (for {
        (value, variable) <- c.lhs
      }yield value*getSolution(varsToGrb(variable))) reduceLeft(_+_)
      if(Operator.leq == c.op){
        println(lhsSum+ " <= " + c.rhs + " : " + (lhsSum>c.rhs))
        lhsSum > c.rhs
      }else{
        println(lhsSum+ " >= " + c.rhs + " : " + (lhsSum>c.rhs))
        lhsSum < c.rhs
      }
    }

    override def callback() {
      if (where == GRB.CB_MIPSOL) {
        val currentObj = getDoubleInfo(GRB.CB_MIPSOL_OBJ)
        if(currentObj<obj && cpiConstraintsInt.size>0){

          println("- CPI Iteration " + iteration)
          // assigning old objective
          // adding violated constraints
          var lazyC:LPConstraint[T] =null
          val reducedCpiConstraints = cpiConstraintsInt.(c => {
            if(isViolatedLazy(c)){
              lazyC=c
              false
            }else true
          }).drop(1)
          //      model.update()
          //      model.write("model"+i+".lp")
          // solving again
          //model.getVars().foreach { v => grbToVars(v).result= v.get(GRB.DoubleAttr.X) }

          // bad scala :-). Re-assigning for next iteration
          obj = currentObj
          iteration = iteration+1
          cpiConstraintsInt = reducedCpiConstraints


          println("- "+ cpiConstraintsInt.size+" violated Constraints remaining ")

          val (expr, op, rhs) = constructConstraint(lazyC)
          addLazy(expr, op, rhs)

        }
      }
    }
  }*/
}



object GurobiILPConnector{
  def main(args: Array[String]) {
    val gurobi = new GurobiILPConnector[String]
    /*val v1 = new Var("hello",  1, Type.binary)
    val v2 = new Var("hello2", 0.9, Type.binary)
    gurobi.addVar(v1).addVar(v2)
      .addConstraint(new LPConstraint[String](List((1,v1),(1,v2)),Operator.leq,1,true))
      .addConstraint(new LPConstraint[String](List((1,v1),(1,v1)),Operator.leq,1,true))
      .addConstraint(new LPConstraint[String](List((1,v2),(1,v2)),Operator.leq,2,true))
    val res = gurobi.solve()
    for{v <- res} yield println(v.name + " = " + v.result)*/

    //val r = new Random(1234)
    val r = new Random(12345)

    val startCPI = System.currentTimeMillis()
    val vars = for{
      i <- (1 to 800)
      v = new Var("v" + i, (r.nextDouble()-0.5), Type.binary)
    } yield v

    vars foreach {v => gurobi.addVar(v)}

    vars foreach {v1 => vars foreach {v2 => if(r.nextDouble()>0.5)
      gurobi.addConstraint(new LPConstraint[String](List((1,v1),(1,v2)),Operator.leq,1,true))}}
    val res = gurobi.solve()
    println("took " + (System.currentTimeMillis()-startCPI))

  }
}
