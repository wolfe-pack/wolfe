package scalapplcodefest.compiler

import scala.tools.nsc.Global

/**
 * User: rockt
 * Date: 1/4/14
 * Time: 10:27 AM
 */

/**
 * Searches for @Differentiable annotation and generates an AST for gradient calculation
 */
class DerivativeTransformer extends WolfeTransformer {
  def transformTree[T <: Global#Tree](global: Global, env: WolfeCompilerPlugin2#Environment, tree: T) = tree match {
    case _ => tree //TODO
  }
}

object DerivativeTransformerPlayground extends App {
  //http://developers.opengamma.com/blog/2013/10/28/deriva-automating-algorithmic-differentiation

  import com.lambder.deriva.Deriva._
  import com.lambder.deriva.Expression

  val sigmoid = div(1.0, add(1.0, exp(mul(-1.0, "z"))))
  val func = sigmoid.function("z")

  println(s"sigmoid:        $sigmoid")
  println(s"sigmoid at 0.5: ${func.execute(0.5)}")

  val result = d[Expression](sigmoid, "z")
  println(s"sigmoid dz:     $result")

  println(result.describe())

}