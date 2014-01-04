package scalapplcodefest.compiler

import scala.tools.nsc.Global
import org.apache.commons.math3.analysis.differentiation.DerivativeStructure

/**
 * User: rockt
 * Date: 1/4/14
 * Time: 10:27 AM
 */

/**
 * Searches for @Differentiable annotations and transforms generates an AST for gradient calculation
 */
class DerivativeTransformer extends WolfeTransformer {
  def transformTree[T <: Global#Tree](global: Global, env: WolfeCompilerPlugin2#Environment, tree: T) = tree match {
    case _ => tree
  }
}

object DerivativeTransformerPlayground extends App {
  val params = 2
  val order = 2
  val xRealValue = 2.5
  val yRealValue = -1.3

  val x = new DerivativeStructure(params, order, 0, xRealValue)
  val y = new DerivativeStructure(params, order, 1, yRealValue)
  val f = DerivativeStructure.hypot(x, y)
  val g = f.log()

  println("g        = " + g.getValue)
  println("dg/dx    = " + g.getPartialDerivative(1, 0))
  println("dg/dy    = " + g.getPartialDerivative(0, 1))
  println("d2g/dx2  = " + g.getPartialDerivative(2, 0))
  println("d2g/dxdy = " + g.getPartialDerivative(1, 1))
  println("d2g/dy2  = " + g.getPartialDerivative(0, 2))
  //have a look at: http://commons.apache.org/sandbox/commons-nabla/
}