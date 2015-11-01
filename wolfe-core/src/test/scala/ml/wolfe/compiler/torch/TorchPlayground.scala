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

  val params = Var[Params]
  val x = Var[Tensor]
  val term = sigmoid(params.W * x + params.b)

  val module = TorchCompiler.compile(term)

  module.init(params := Params(DenseMatrix.ones(2, 2), DenseMatrix(1.0, 0.0)))
  module.forward(x := DenseMatrix(1.0, 2.0))

  println(module.output())



}
