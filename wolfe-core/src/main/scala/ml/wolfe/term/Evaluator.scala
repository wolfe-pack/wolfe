package ml.wolfe.term

import ml.wolfe.Language
import org.scalactic._
import Accumulation._
import Language._

/**
 * @author riedel
 */
object BaseEval extends Evaluator {

  implicit class EvaluatorTerm[T](val term: Term[T]) {
    def eval(bindings: Binding[Any]*) = BaseEval.eval(Bindings(bindings: _*))(term)
  }

  def partial[T](bindings: Bindings, backoff: Evaluator) = {
    def e[A](term: Term[A]) = backoff.eval(bindings)(term)
    val result: PartialFunction[Term[T], T Or Every[WolfeError]] = {

      case Constant(t) =>
        Good(t)

      case v: Var[_] => bindings.get(v) match {
        case Some(value) => Good(value)
        case None => Bad(One(VariableNotBound(v)))
      }

      case p: Plus[_] =>
        for (v1 <- e(p.arg1);
             v2 <- e(p.arg2)) yield
        p.numeric.plus(v1, v2)

      case p: Times[_] =>
        for (v1 <- e(p.arg1);
             v2 <- e(p.arg2)) yield
        p.numeric.times(v1, v2)

      case SeqApply(s, i) =>
        for (vs <- e(s);
             vi <- e(i)) yield
        vs(vi)

      case SeqLength(s) =>
        for (vs <- e(s)) yield vs.length

      case SeqMap(s, f) =>
        for (vs <- e(s);
             va <- vs.map(a => eval(bindings + (f.argument := a))(f.body)).combined)
          yield va

      case SeqSlice(s, from, to) =>
        for (vs <- e(s); vfrom <- e(from); vto <- e(to)) yield
        vs.slice(vfrom, vto)

      case SeqFill(length, elem) =>
        for (vlength <- e(length); velem <- e(elem)) yield
        Seq.fill(vlength)(velem)

      case GetElement(p, index) =>
        for (vp <- e(p)) yield
        vp.productElement(index).asInstanceOf[T]

      case SeqConstructor(args) =>
        for (va <- args.map(eval(bindings)).combined) yield
        va

      case ConstructProduct(args, c) =>
        for (va <- args.map(e).combined) yield
        c(va)

      case term => Bad(One(TermNotSupported(term)))

    }
    result
  }
}

object BaseEvalTest {
  def main(args: Array[String]) {
    import BaseEval._
    implicit val domains = new Domains
    val Ints = RangeDom(0 until 3)
    val i = Ints.Variable("i")
    println(eval(Bindings(i := 1))(i + i))

    val s = Var[Seq[Double]]("s")
    println(eval(s := Seq(1.0, 2.0))(s map (_ + 2.0)))

  }
}

trait Evaluator {

  def partial[T](bindings: Bindings, backoff: Evaluator): PartialFunction[Term[T], T Or Every[WolfeError]]

  def eval[T](bindings: Bindings)(term: Term[T]) =
    partial(bindings, this)(term)

  def eval[T](bindings: Binding[Any]*)(term: Term[T]): T Or Every[WolfeError] =
    eval(Bindings(bindings: _*))(term)

  def +(that: Evaluator) = new Evaluator {
    def partial[T](bindings: Bindings, backoff: Evaluator) =
      partial[T](bindings,backoff) orElse that.partial[T](bindings,backoff)
  }

}

abstract class WolfeError extends RuntimeException {
  def msg: String
  override def getMessage = msg
}

abstract class TermErrorMsg extends WolfeError {
  def term: Term[Any]
}

case class VariableNotBound(variable: Var[Any]) extends TermErrorMsg {
  def term = variable

  def msg = s"Variable $variable is not bound"
}

case class TermNotSupported(term: Term[Any]) extends TermErrorMsg {
  def msg = s"Not supported: $term"
}


trait Bindings extends Iterable[Binding[Any]] {

  def apply[T](variable: Var[T]) = get(variable).get

  def get[T](variable: Var[T]): Option[T]

  def contains[T](variable: Var[T]) = get(variable).isDefined

  def +[T](binding: Binding[T]): Bindings

  def ++(bindings: Bindings): Bindings
}

object Bindings {
  def apply(bindings: Binding[Any]*): Bindings = {
    val result = new MutableBindings
    for (b <- bindings) result(b.variable) = b.value
    result
  }
}


class MutableBindings extends Bindings {
  private var map: Map[Var[Any], Any] = Map.empty

  def update[T](variable: Var[T], value: T): Unit = {
    map += (variable -> value)
  }

  def get[T](variable: Var[T]) = map.get(variable).asInstanceOf[Option[T]]

  def +[T](binding: Binding[T]) = {
    val result = new MutableBindings
    result.map = map + (binding.variable -> binding.value)
    result
  }


  def ++(bindings: Bindings) = {
    val result = new MutableBindings
    result.map = map ++ (bindings.map(b => b.variable -> b.value)).toMap
    result

  }

  def iterator = map.iterator.map(p => Binding[Any](p._1,p._2))


}

case class Binding[+T](variable: Var[T], value: T)


