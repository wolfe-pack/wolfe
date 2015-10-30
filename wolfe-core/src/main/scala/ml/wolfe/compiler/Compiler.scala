package ml.wolfe.compiler

import ml.wolfe.term.Term

/**
 * @author riedel
 */
trait Compiler {
  def compile[T](term:Term[T]) : Module [T]

}
