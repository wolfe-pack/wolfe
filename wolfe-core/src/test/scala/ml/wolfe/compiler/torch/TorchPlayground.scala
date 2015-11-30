package ml.wolfe.compiler.torch

import ml.wolfe._
import ml.wolfe.term.termdef
import org.nd4s.Implicits._

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

  module.init(params := Params(Seq.fill(1)(4).asNDArray(2,2), Seq(1.0, 0.0).asNDArray()))
  module.forward(input := Input(Seq(1.0, 2.0).asNDArray()))

  println(module.output())

  module.backward(Seq(1.0, 2.0).asNDArray())

  val gradient = module.gradient(params)
  println(gradient)

  println(module.param(params))


}
