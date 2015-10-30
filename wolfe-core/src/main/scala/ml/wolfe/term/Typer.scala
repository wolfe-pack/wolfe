package ml.wolfe.term

import ml.wolfe.Language
import ml.wolfe.term.Typer.TyperError
import org.scalautils.Accumulation._
import org.scalautils._

/**
 * @author riedel
 */
object Typer {

  import Language._


  case class TypedTerm[T](term: Term[T], dom: Dom[T])


  def domain[T](domains: DomainBinding[Any]*)(term: Term[T]): Dom[T] Or Every[ErrorMsg] =
    domain(Domains(domains: _*))(term)

  def domain[T](domains: Domains)(term: Term[T]): Dom[T] Or Every[ErrorMsg] = term match {
    case v: Var[_] =>
      domains.get(v) match {
        case Some(d) => Good(d)
        case None => Bad(One(VariableNotBound(v)))
      }

    case SeqApply(s, _) =>
      for (d <- domain(domains)(s);
           SeqDom(e, _, _) = d) yield e

    case SeqAppend(s, _) =>
      for (d <- domain(domains)(s);
           SeqDom(e, min, max) = d) yield SeqDom(e, min + 1, max + 1)

    case ConstructProduct(args, constructor) =>
      for (argDoms <- args.map(domain(domains)).combined) yield ProductDom(argDoms, constructor)

    case GetElement(product, index) =>
      for (dom <- domain(domains)(product);
           ProductDom(doms, _) = dom) yield doms(index).asInstanceOf[Dom[T]]

    case SeqSlice(s, from, to) =>
      for (dom <- domain(domains)(s);
           SeqDom(e, min, max) = dom) yield SeqDom(e, min, max) //todo can be tighter

    case Plus(a1, a2) =>
      for (d1 <- domain(domains)(a1);
           d2 <- domain(domains)(a2)) yield {
        (d1, d2) match {
          case (RangeDom(r1), RangeDom(r2)) => RangeDom(Range(r1.start + r2.start, r1.end + r2.end))
        }
      }
  }

  case class TyperError(term: Term[Any], domains: Domains, msg: String) extends ErrorMsg

  def check[T, A, E <: ErrorMsg](value: T, error: E)(fun: PartialFunction[T, A]): A Or One[E] = {
    if (fun.isDefinedAt(value)) Good(fun(value)) else Bad(One(error))
  }

  def domains(varDoms: Domains)(term: Term[Any]): Domains Or Every[ErrorMsg] = {
    def dom(term: Term[Any]) = domains(varDoms)(term)
    term match {
      case v: Var[_] =>
        varDoms.get(v) match {
          case Some(_) => Good(varDoms)
          case None => Bad(One(VariableNotBound(v)))
        }
      case sa@SeqApply(s, i) =>
        for (ds <- dom(s);
             di <- dom(i);
             elemDom <- check(ds(s), TyperError(s, ds, "not a sequence domain")) { case SeqDom(eD, _, _) => eD })
          yield (ds ++ di) + (sa in elemDom)

      case ge@GetElement(p, e) =>
        for (dp <- dom(p);
             de <- dom(e);
             doms <- check(dp(p), TyperError(p, dp, "not a product domain")) { case ProductDom(d, _) => d })
          yield (dp ++ de) + (ge in doms(e))

      case tm@TensorMul(x1, x2) =>
        for (dx1 <- dom(x1);
             dx2 <- dom(x2);
             dims1 <- check(dx1(x1), TyperError(x1, dx1, "not a tensor domain")) { case TensorDom(d) => d };
             dims2 <- check(dx2(x2), TyperError(x2, dx2, "not a tensor domain")) { case TensorDom(d) => d };
             dims <- if (dims1(1) == dims2(0))
               Good(List(dims1(0), dims2(1))) else
               Bad(One(TyperError(tm, dx1 ++ dx2, s"domains don't match: $dims1 * $dims2"))))
          yield (dx1 ++ dx2) + (tm in TensorDom(dims))


    }
  }


  def main(args: Array[String]) {

    import Language._

    val i = Var[Int]("i")

    println(domain(i in RangeDom(0 until 3))(i + i))
  }

  implicit class TypeTerm[T](val term: Term[T]) {
    def dom(bindings: DomainBinding[Any]*) = Typer.domain(Domains(bindings: _*))(term)

    def dom(implicit domains: Domains) = Typer.domain(domains)(term)

  }


}

class Domains {
  private var map: Map[Term[Any], Dom[Any]] = Map.empty

  def get[T](term: Term[T]): Option[Dom[T]] = map.get(term).asInstanceOf[Option[Dom[T]]]

  def update[T](term: Term[T], dom: Dom[T]) = {
    map += (term -> dom)
  }

  def apply[T](term: Term[T]) = get(term).get

  def +(binding: DomainBinding[Any]) = {
    val result = new Domains
    result.map = map + (binding.term -> binding.dom)
    result
  }

  def ++(domains: Domains) = {
    val result = new Domains
    result.map = map ++ domains.map
    result
  }

  def check[D <: Dom[Any]](term:Term[Any]): D Or One[ErrorMsg] = {
    try {
      Good(this(term).asInstanceOf[D])
    } catch {
      case c:ClassCastException => Bad(One(TyperError(term, this, "Expected different domain type")))
      case c:NoSuchElementException => Bad(One(TyperError(term, this, "domain not assigned")))
    }
  }

}

case class DomainBinding[+T](term: Term[T], dom: Dom[T])

object Domains {
  def apply(bindings: DomainBinding[Any]*) = {
    val r = new Domains
    r.map = bindings.map(b => b.term -> b.dom).toMap
    r
  }
}