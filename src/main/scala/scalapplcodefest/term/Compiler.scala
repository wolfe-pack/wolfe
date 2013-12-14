package scalapplcodefest.term

/**
 * @author Sebastian Riedel
 */
object Compiler {

}

trait Hint

case class Annotation[T](self:Term[T],hint:Hint) extends ProxyTerm[T] with Composite1[T,T] {
  def components = self
  def copy(t1: Term[T]) = Annotation(t1,hint)
}
