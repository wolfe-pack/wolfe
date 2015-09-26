package ml.wolfe.term.simplified

import org.scalautils._
import Accumulation._

/**
 * @author riedel
 */
object BaseEval extends Evaluator {

  import Wolfe._

  implicit class EvaluatorTerm[T](val term: STerm[T]) {
    def eval(bindings: Binding[Any]*) = BaseEval.eval(Bindings(bindings: _*))(term)
  }

  def partial[T](bindings: Bindings, backoff:Evaluator) = {
    def e[A](term: STerm[A]) = backoff.eval(bindings)(term)
    val result: PartialFunction[STerm[T], T Or Every[ErrorMsg]] = {

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
        var variable = new Var[Any]("_tmp")
        val body = f(variable)
        for (vs <- e(s);
             va <- vs.map(a => eval(bindings + (variable := a))(body)).combined)
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
    import Wolfe._
    import BaseEval._
    implicit val domains = new Domains
    val Ints = RangeDom(0 until 3)
    val i = Ints.Variable("i")
    println(eval(Bindings(i := 1))(i + i))

    val s = Var[Seq[Double]]
    println(eval(s := Seq(1.0, 2.0))(s map (_ + 2.0)))

  }
}

trait Evaluator {

  def partial[T](bindings: Bindings, backoff: Evaluator): PartialFunction[STerm[T], T Or Every[ErrorMsg]]

  def eval[T](bindings: Bindings)(term: STerm[T]) =
    partial(bindings, this)(term)

  def eval[T](bindings: Binding[Any]*)(term: STerm[T]): T Or Every[ErrorMsg] =
    eval(Bindings(bindings: _*))(term)

  def +(that: Evaluator) = new Evaluator {
    def partial[T](bindings: Bindings,backoff:Evaluator) =
      partial[T](bindings,backoff) orElse that.partial[T](bindings,backoff)
  }

}

trait ErrorMsg {
  def msg: String

  def term: STerm[Any]
}

case class VariableNotBound(variable: Var[Any]) extends ErrorMsg {
  def term = variable

  def msg = s"Variable $variable is not bound"
}

case class TermNotSupported(term: STerm[Any]) extends ErrorMsg {
  def msg = s"Not supported: $term"
}


trait Bindings {
  def get[T](variable: Var[T]): Option[T]

  def +[T](binding: Binding[T]): Bindings
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
}

case class Binding[+T](variable: Var[T], value: T)


