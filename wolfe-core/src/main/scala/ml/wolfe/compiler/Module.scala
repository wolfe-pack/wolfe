package ml.wolfe.compiler

import ml.wolfe.term.{Binding, Var}

/**
 * @author riedel
 */
trait Module[T] {
  def gradient[G](param: Var[G]): G

  def param[P](param:Var[P]):P

  def backward(output: T)

  def output(): T

  def forward(bindings: Binding[Any]*)

  def init(bindings: Binding[Any]*)

}
