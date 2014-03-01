package scalapplcodefest.sbt

//import scalapplcodefest.Wolfe.Objective.{Wolferine, Differentiable} //rockt: why does this fail?

/**
 * Created by rockt on 07/01/2014.
 */
object ExampleDifferentiableTemplate {
  def sigmoid(z: Double) = 1 / (1 + math.exp(-z))
}
