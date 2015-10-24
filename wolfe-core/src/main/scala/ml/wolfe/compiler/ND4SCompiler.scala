package ml.wolfe.compiler

import ml.wolfe.term.{Var, Tensor, Binding, Term}

/**
 * @author rockt
 */
object ND4SCompiler {
  def compile[T](term: Term[T]):Module[T] = ???

}

trait Module[T] {
  def gradient[G](param: Var[G]):G = ???

  def backward(output:T)

  def output():T = ???

  def forward(binding: Binding[Tensor]) = ???

  def init(bindings: Binding[Tensor]*) = ???

}