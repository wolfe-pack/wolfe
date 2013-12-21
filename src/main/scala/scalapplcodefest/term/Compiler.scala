package scalapplcodefest.term

import scala.language.implicitConversions
import scalapplcodefest.TermConverter
import scalapplcodefest.TermConverter.Converter
import scalapplcodefest.value.{Argmin, Argmax}

/**
 * @author Sebastian Riedel
 */
object Compiler {

  def compile[T](term: Term[T]): Term[T] = TermConverter.convertChildrenFirst(term) {
    implicit def terribleCastMagicForPatternMatchingWithGenerics[A, B](from: Term[A]) = from.asInstanceOf[Term[B]]
    new Converter {
      def convert[A](toConvert: Term[A]) = toConvert match {
        case FunApp(RestrictedFun(Argmax, _, _), LambdaAbstraction(sig, Annotation(objective, a: MaxHint))) =>
          a.withoutParam(sig, objective).argmax
        case t => t
      }
    }
  }

}

trait CompilerHint

case class Annotation[T](self: Term[T], hint: CompilerHint) extends ProxyTerm[T] with Composite1[T, T] {
  def components = self
  def copy(t1: Term[T]) = Annotation(t1, hint)
}

