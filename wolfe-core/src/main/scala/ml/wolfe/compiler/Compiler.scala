package ml.wolfe.compiler

import ml.wolfe.term.Term

/**
  * A compiler compiles terms into modules that support computation with respect to the term.
  * @author riedel
  */
trait Compiler {
  def compile[T](term: Term[T]): Module[T]
}
