package ml.wolfe.compiler

import ml.wolfe.term.{Bindings, Binding, Var}

/**
 * @author riedel
 */
trait Module[T] {
  def gradient[G](param: Var[G]): G

  def param[P](param: Var[P]): P

  def backward(output: T)

  def output(): T

  def forward(bindings: Binding[Any]*)

  def init(bindings: Binding[Any]*)

  def train(data: Seq[(Bindings, T)], params: TrainParams) = ???

}

case class TrainParams(iterations: Int, learningRate: Double)