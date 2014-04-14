package ml.wolfe.ilp

import ml.wolfe.FactorGraph
import ml.wolfe.FactorGraph.{Factor, Node}
import ml.wolfe.ilp._

/**
 * Created by Jan on 05.01.14.
 */
object MPwithILP{

  /**
   * Constructs an ILP
   * @param fg the message passing graph to run
   */
  def apply(fg: FactorGraph) {
    val connector = new GurobiILPConnector[NodeOrFactorValue]()


    val nodeVariables = new scala.collection.mutable.HashMap[Node, Array[NodeOrFactorValue]]()

    // Create binary ILP variables for every state of a node.
    for{node <- fg.nodes} yield {
      val dim = node.dim
      val nodeOrFactors = new Array[NodeOrFactorValue](dim)
      val nodeStates = (for {i <- 0 to (dim-1)} yield {
        // Create the ILP Variable
        nodeOrFactors(i) = NodeOrFactorValue(node, null, i, 0, connector);
        (1d,nodeOrFactors(i).ilpVar)
      }).toList
      // Create Constraint: Nodes may have only one state.
      // e1 + e2 + e3 + ... <= 1
      // connector.addConstraint(new ILPConstraint[NodeOrFactorValue](allEntriesOfTable, ILPOperator.leq, 1, false))
      connector.addConstraint(new ILPConstraint(nodeStates,ILPOperator.leq,1,false))
      nodeVariables(node) = nodeOrFactors
    }

    // Create ILP Variables for each entry in factor table
    // and create constraints.
    for{factor <- fg.factors
    } yield {
      val tableIdentifiers = factor.settings
      val tableValues = factor.table

      val allEntriesOfTable:List[(Double,ILPVar[NodeOrFactorValue])] = (for ((setting, index1) <- tableIdentifiers.zipWithIndex) yield{
        val weight = tableValues(index1)
        // Create the ILP Variable
        val tableEntry = new NodeOrFactorValue(null,factor,index1,weight,connector)

        // Create Constraints: Link m feature states n_1,n_2,...,n_m with  feature  f
        // n_1 ^ n_2 ^ ... ^ n_m <=> f
        // force f to be 1 if all n_i are 1.
        // + n1 + n2 + ... + n_m <= m - 1 + f
        // force all n_i to be 1 if f is 1.
        // n1 + n2 + ... + n_m >= m * f

        val nodeStatesForEntry = {for((nodePos, index2) <- setting.zipWithIndex) yield{
          val nodeStateForEntry = nodeVariables.get(factor.edges(index2).n).get(nodePos)
          (1d, nodeStateForEntry.ilpVar)
        }}.toList
        val m = factor.edges.length.toDouble
        // add n1 + n2 + ... + n_m - f <= m-1. If weight > 0 we add the rule immediately; else we apply CPI.
        connector.addConstraint(new ILPConstraint[NodeOrFactorValue](nodeStatesForEntry :+(-1d, tableEntry.ilpVar), ILPOperator.leq,(m-1),!(weight>0))) //
        // add n1 + n2 + ... + n_m - m * f >= 0. If weight < 0 we add the rule immediately; else we apply CPI.
        connector.addConstraint(new ILPConstraint[NodeOrFactorValue](nodeStatesForEntry :+((-m), tableEntry.ilpVar), ILPOperator.geq,0,!(weight<0))) //

        (1d, tableEntry.ilpVar)
      }).toList
      // Create Constraint: Only one entry is allowed to be selected! (not neccessary)
      // e1 + e2 + e3 + ... <= 1
      // connector.addConstraint(new ILPConstraint[NodeOrFactorValue](allEntriesOfTable, ILPOperator.leq, 1, false))

      val index = factor.index
      val dims = factor.dims
    }

    // solve
    val results = connector.solve().get

    // store result of each node in node.b (= belief) array
    results foreach {result => {
        val nodeOrFeatureEntry = result.name
        val node = nodeOrFeatureEntry.node
        if(node != null){
          val belief = result.result
          node.b(nodeOrFeatureEntry.position) = belief
        }
      }
    }
    connector.close()

  }

}


  /**
   * This class represents a value (table entry, state, etc.) of a node or a feature. This class will serve as ILP variable.
   *
   * @param node If it represents a node value, this must contain a node. Else null.
   * @param factor If it represents a factor value, this must contain a factor. Else null. Factor weight must be set in this case.
   * @param position Integer position of the value (table entry, state) starting with 0.
   * @param factorWeight Must be set for factors. For Nodes, it can be left at its default value 0.
   */
  case class NodeOrFactorValue(node: Node = null, factor: Factor = null, position: Int, factorWeight:Double = 0, connector: ILPConnector[NodeOrFactorValue]){
    val ilpVar :ILPVar[NodeOrFactorValue]={
      val variable = new ILPVar[NodeOrFactorValue](this,factorWeight, ILPType.binary)
      connector.addVar(variable)
      variable
    }
  }



