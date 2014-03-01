package ml.wolfe

import ml.wolfe.legacy.TermDSL._
import org.scalatest.{Matchers, WordSpec}
import ml.wolfe.ilp._
import ml.wolfe.ilp.ILPConstraint

/**
 * Created by Jan on 06.01.14.
 */
class ILPSpecs extends WordSpec with Matchers{
  // TODO Enable spec, when Gurobi is installed.
  /**
  "An simple ILP with objective (max 1x + 0.9 y), x,y in {0,1} and non-cpi constraint (x+y<=1) term" should {
    "return (x=1, y=0)" in {
      val gurobi = new GurobiILPConnector[String]
      val v1 = new ILPVar("x",  1, ILPType.binary)
      val v2 = new ILPVar("y", 0.9, ILPType.binary)
      gurobi.addVar(v1).addVar(v2)
        .addConstraint(new ILPConstraint[String](List((1,v1),(1,v2)),ILPOperator.leq,1,false))
      gurobi.solve().get
      //for{v <- res} yield println(.name + " = " + v.result)

      v1.result should be(1)
      v2.result should be(0)
    }
  }

  "An simple ILP with objective (max 1x + 0.9 y), x,y in {0,1} and cpi constraint (x+y<=1) term" should {
    "return (x=1, y=0)" in {
      val gurobi = new GurobiILPConnector[String]
      val v1 = new ILPVar("x",  1, ILPType.binary)
      val v2 = new ILPVar("y", 0.9, ILPType.binary)
      gurobi.addVar(v1).addVar(v2)
        .addConstraint(new ILPConstraint[String](List((1,v1),(1,v2)),ILPOperator.leq,1,true))
      gurobi.solve().get
      //for{v <- res} yield println(.name + " = " + v.result)

      v1.result should be(1)
      v2.result should be(0)
    }
  }

  "A unbounded ILP with objective (max 1x + 0.9 y), x,y integer " should {
    "return a bad result." in {
      val gurobi = new GurobiILPConnector[String]
      val v1 = new ILPVar("x",  1, ILPType.int)
      val v2 = new ILPVar("y", 0.9, ILPType.int)
      gurobi.addVar(v1).addVar(v2)
      val result = gurobi.solve()
      //for{v <- res} yield println(.name + " = " + v.result)
      result.isBad should be(true)
    }
  }

  "An infeasible ILP with objective (max 1x + 0.9 y), x,y in {0,1} and cpi constraints (x+y<=1) and (x+y>=2) term" should {
    "return a bad result" in {
      val gurobi = new GurobiILPConnector[String]
      val v1 = new ILPVar("x",  1, ILPType.binary)
      val v2 = new ILPVar("y", 0.9, ILPType.binary)
      gurobi.addVar(v1).addVar(v2)
        .addConstraint(new ILPConstraint[String](List((1,v1),(1,v2)),ILPOperator.leq,1,true))
        .addConstraint(new ILPConstraint[String](List((1,v1),(1,v2)),ILPOperator.geq,2,true))
      val result = gurobi.solve()
      //for{v <- res} yield println(.name + " = " + v.result)
      result.isBad should be(true)
    }
  }
  */
}
