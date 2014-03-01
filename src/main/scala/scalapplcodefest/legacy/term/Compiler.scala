package scalapplcodefest.legacy.term

import scala.language.implicitConversions
import scalapplcodefest.legacy.GradientBasedArgminHint
import scalapplcodefest.legacy.value.{MaxValue, Argmin, Argmax}

/**
 * @author Sebastian Riedel
 */
object Compiler {

  def compile[T](term: Term[T], param: Option[Variable[scalapplcodefest.Vector]] = None): Term[T] = {
    implicit def terribleCastMagicForPatternMatchingWithGenerics[A, B](from: Term[A]) = from.asInstanceOf[Term[B]]

    term match {
      case FunApp(RestrictedFun(Argmax, _, _), LambdaAbstraction(sig, Annotation(objective, a: MaxHint))) =>
        param match {
          case Some(w) => a.withParam(sig, w, objective).argmax
          case None => a.withoutParam(sig, objective).argmax
        }
      case FunApp(RestrictedFun(MaxValue, _, _), LambdaAbstraction(sig, Annotation(objective, a: MaxHint))) =>
        param match {
          case Some(w) => a.withParam(sig, w, objective)
          case None => a.withoutParam(sig, objective)
        }
      case FunApp(RestrictedFun(Argmin, _, _), LambdaAbstraction(VarSig(w), Annotation(objective, hint: GradientBasedArgminHint))) =>
        val parameter = w.asInstanceOf[Variable[scalapplcodefest.Vector]]
        val compiledObjective = compile(objective,Some(parameter))
        //todo: this requires that parameter is the only free variable in the term
        Constant(hint.minimize(parameter,compiledObjective).asInstanceOf[T])
      case c:Composite[_] => c.copySeq(c.componentSeq.map(a => compile(a,param)))
    }

  }
}

trait CompilerHint

case class Annotation[T](self: Term[T], hint: CompilerHint) extends ProxyTerm[T] with Composite1[T, T] {
  def components = self
  def copy(t1: Term[T]) = Annotation(t1, hint)
  override def variables = super[ProxyTerm].variables
}

