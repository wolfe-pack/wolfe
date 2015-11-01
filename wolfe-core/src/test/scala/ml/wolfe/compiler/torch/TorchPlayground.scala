package ml.wolfe.compiler.torch

import breeze.linalg.DenseMatrix
import ml.wolfe._
import ml.wolfe.term.termdef

/**
 * @author riedel
 */
object TorchPlayground extends App {

  import Language._


  @termdef case class Params(W: Tensor, b: Tensor)
  @termdef case class Input(x: Tensor)


  val params = Var[Params]
  val input = Var[Input]
  val term = sigmoid(params.W * input.x + params.b)

  val module = TorchCompiler.compile(term)

  module.init(params := Params(DenseMatrix.ones(2, 2), DenseMatrix(1.0, 0.0)))
  module.forward(input := Input(DenseMatrix(1.0, 2.0)))

  println(module.output())

  module.backward(DenseMatrix(1.0, 2.0))

  val gradient = module.gradient(params)
  println(gradient)

  println(module.param(params))


}
