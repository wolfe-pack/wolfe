package ml.wolfe.fg

import breeze.linalg._
import ml.wolfe.{BeliefPropagation, FactorGraph}
import ml.wolfe.FactorGraph.{Edge, Node}

import scala.collection.mutable


/**
 * @author Sebastian Riedel
 */
object KernelBP {

  def main(args: Array[String]) {
    val data_AB = for (i <- 0 until 4) yield (i, 3 - i)
    val data_BC = for (i <- 0 until 4) yield (i, 3 - i)
    val data_AB_A = data_AB.map(_._1)
    val data_AB_B = data_AB.map(_._2)
    val data_BC_B = data_BC.map(_._1)
    val data_BC_C = data_BC.map(_._2)

    val data_A = data_AB_A
    val data_B = data_BC_B

    val kernel_A: (Any, Any) => Double = { case (x1, x2) => if (x1 == x2) 1.0 else 0.0 }
    val kernel_B: (Any, Any) => Double = { case (x1, x2) => if (x1 == x2) 1.0 else 0.0 }
    val kernel_C: (Any, Any) => Double = { case (x1, x2) => if (x1 == x2) 1.0 else 0.0 }

    val model_AB = new EdgeModel(data_AB, kernel_A, kernel_B, 0.1)
    val model_BC = new EdgeModel(data_BC, kernel_B, kernel_C, 0.1)

    //define translations between messages from edges
    val trans_AB_BC = calculateTranslationMatrix(data_AB_B, data_BC_B, kernel_B)
    val trans_BC_AB = trans_AB_BC.t
    val trans_AB_B = calculateTranslationMatrix(data_AB_B, data_B, kernel_B)

    val f_trans_AB_B = (d: DenseVector[Double]) => trans_AB_B * d
    val f_trans_AB_A = (d: DenseVector[Double]) => d
    val f_trans_BC_B = (d: DenseVector[Double]) => d
    val f_trans_AB_BC = (d: DenseVector[Double]) => trans_AB_BC * d
    val f_trans_BC_AB = (d: DenseVector[Double]) => trans_BC_AB * d

    //create the factor graph
    val fg = new FactorGraph
    val n_A = fg.addNode(0)
    val n_B = fg.addNode(0)
    //val n_C = fg.addNode(0) //we don't use this node because it's observed

    val f_AB = fg.addFactor()
    val f_BC = fg.addFactor()

    val e_BC_B = fg.addEdge(f_BC, n_B)
    val e_AB_B = fg.addEdge(f_AB, n_B)
    val e_AB_A = fg.addEdge(f_AB, n_A)

    e_BC_B.msgs = new KernelBPMSgs
    e_AB_B.msgs = new KernelBPMSgs
    e_AB_A.msgs = new KernelBPMSgs

    val obs_C = 2

    f_AB.potential = new KernelBPPairPotential(e_AB_A, e_AB_B, model_AB)
    f_BC.potential = new KernelBPLocalPotential(model_BC, obs_C)

    n_A.variable = new KernelBPVar(Map.empty, Map(e_AB_A -> f_trans_AB_A), data_A.size)
    n_B.variable = new KernelBPVar(
      Map(
        (e_AB_B, e_BC_B) -> f_trans_AB_BC,
        (e_BC_B, e_AB_B) -> f_trans_BC_AB),
      Map(
        e_AB_B -> f_trans_AB_B,
        e_BC_B -> f_trans_BC_B
      ), data_B.size)

    fg.build()

    BeliefPropagation.sumProduct(1,gradientAndObjective = false,schedule = false)(fg)
    println(n_A.variable.asInstanceOf[KernelBPVar].belief)
    println(n_B.variable.asInstanceOf[KernelBPVar].belief)

  }

  def normalizeMsg(msg: DenseVector[Double]) {
    val sum = msg.sum
    if (math.abs(sum) > 0.0)
      msg :*= 1.0 / sum
    else
      msg := 1.0 / msg.length
  }

  def calculateTranslationMatrix(sourceData: Seq[Any], targetData: Seq[Any], targetKernel: (Any, Any) => Double) = {
    val result = new DenseMatrix[Double](targetData.size, sourceData.size)
    for (col <- 0 until sourceData.size; row <- 0 until targetData.size) {
      result(row, col) = targetKernel(sourceData(col), targetData(row))
    }
    result
  }


  class KernelBPVar(val edge2edgeTranslations: Map[(Edge, Edge), DenseVector[Double] => DenseVector[Double]],
                    val edge2nodeTranslation: Map[Edge, DenseVector[Double] => DenseVector[Double]],
                    val dim: Int) extends Var {
    var belief: DenseVector[Double] = null


    override def updateN2F(edge: Edge) = {
      val msgs = edge.msgs.asInstanceOf[KernelBPMSgs]
      msgs.n2f = new DenseVector[Double](Array.fill(dim)(1.0))
      for (other <- edge.n.edges; if other != edge) {
        val source = other.msgs.asInstanceOf[KernelBPMSgs]
        val translation = edge2edgeTranslations(other, edge)
        msgs.n2f :*= translation(source.f2n)
      }
      normalizeMsg(msgs.n2f)
    }
    override def updateMarginalBelief(node: Node) = {
      belief = new DenseVector[Double](Array.fill(dim)(1.0))
      for (edge <- node.edges) {
        val source = edge.msgs.asInstanceOf[KernelBPMSgs]
        val translation = edge2nodeTranslation(edge)
        belief :*= translation(source.f2n)
      }
      normalizeMsg(belief)

    }
  }

  class KernelBPMSgs extends Msgs {
    var f2n: DenseVector[Double] = null
    var n2f: DenseVector[Double] = null

    var f2nOld: DenseVector[Double] = null
    def saveCurrentF2NAsOld() = f2nOld = f2n
  }

  def gram(kernel: (Any, Any) => Double, data: Seq[(Any, Any)], part: ((Any, Any)) => Any) = {
    val G1 = new DenseMatrix[Double](data.size, data.size)
    for (i <- 0 until data.size) {
      val x1 = part(data(i))
      for (j <- i until data.size) {
        val y1 = part(data(j))
        val sim1 = kernel(x1, y1)
        G1(i, j) = sim1
        G1(j, i) = sim1
      }
    }
    G1
  }

  class EdgeModel(val data: Seq[(Any, Any)],
                  val k1: (Any, Any) => Double, val k2: (Any, Any) => Double,
                  val lambda: Double) {

    val G1    = gram(k1, data, _._1)
    val G2    = gram(k2, data, _._2)
    val R     = DenseMatrix.eye[Double](data.size) * lambda * data.size.toDouble
    val G1_R  = G1 + R
    val G2_R  = G2 + R
    val T1    = LinearAlgebra.inv(G1_R)
    val T2    = LinearAlgebra.inv(G2_R)
    val Obs12 = LinearAlgebra.inv(G1_R * G2_R)
    val Obs21 = LinearAlgebra.inv(G2_R * G1_R)


  }

  trait KernelBPPotential extends Potential {
    def model: EdgeModel
  }

  class KernelBPPairPotential(val arg1: Edge, val arg2: Edge, val model: EdgeModel) extends KernelBPPotential {


    override def marginalF2N(edge: Edge) = {
      val (other, trans) = if (edge == arg1) (arg2, model.T1) else (arg1, model.T2)
      val pre = other.msgs.asInstanceOf[KernelBPMSgs]
      val msgs = edge.msgs.asInstanceOf[KernelBPMSgs]
      msgs.f2n = trans * pre.n2f
    }
  }


  class KernelBPLocalPotential(val model: EdgeModel, val observation: Any) extends KernelBPPotential {

    val orig = new DenseVector[Double](Array.ofDim[Double](model.data.size))
    for (((x1, x2), i) <- model.data.zipWithIndex) orig(i) = model.k2(x2, observation)
    val msg = model.Obs21 * orig

    override def marginalF2N(edge: Edge) = {
      val msgs = edge.msgs.asInstanceOf[KernelBPMSgs]
      msgs.f2n = msg
    }


  }

}
