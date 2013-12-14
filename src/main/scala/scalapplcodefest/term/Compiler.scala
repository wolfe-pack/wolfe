package scalapplcodefest.term

/**
 * @author Sebastian Riedel
 */
object Compiler {

  def compile[T](term:Term[T]):Term[T] = {
    ???
  }

}

trait CompilerHint

case class Annotation[T](self:Term[T],hint:CompilerHint) extends ProxyTerm[T] with Composite1[T,T] {
  def components = self
  def copy(t1: Term[T]) = Annotation(t1,hint)
}
