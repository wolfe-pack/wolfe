package ml.wolfe.term

import ml.wolfe.{Tensor, Language}
import ml.wolfe.term.Typer.TyperError
import org.nd4j.linalg.api.ndarray.INDArray
import org.scalactic.Accumulation._
import org.scalactic._
import ml.wolfe.compiler.nd4s.PimpMyND4S._

/**
  * @author riedel
  */
object Typer {

  import Language._

  case class TyperError(term: Term[Any], domains: Domains, msg: String) extends WolfeError

  case class CantDeriveDomainFromValue(value: Any) extends WolfeError {
    val msg = "Can't derive domain for value: " + value
  }

  def deriveDomainFromValue[T](value: T): Dom[T] = {
    val result = value match {
      case i: Int => Ints
      case d: Double => Doubles
      case d: Tensor =>
        TensorDom(List(d.rows, d.cols))
      case s: Seq[_] =>
        val argDom = deriveDomainFromValue(s.head)
        SeqDom(argDom, s.length, s.length)
      case p: Product =>
        val argDoms = p.productIterator.toList.map(deriveDomainFromValue)
        val clazz = p.getClass
        val copyMethod = clazz.getMethods.find(_.getName == "copy").get
        var currentPrototype = p
        def construct(args: Seq[Any]) = {
          currentPrototype = copyMethod.invoke(p, args.asInstanceOf[Seq[AnyRef]]: _*).asInstanceOf[Product]
          currentPrototype
        }
        ProductDom(argDoms, construct)
    }
    result.asInstanceOf[Dom[T]]
  }


  def check[T, A, E <: WolfeError](value: T, error: E)(fun: PartialFunction[T, A]): A Or One[E] = {
    if (fun.isDefinedAt(value)) Good(fun(value)) else Bad(One(error))
  }

  def domains(varDoms: DomainBinding[Any]*)(term: Term[Any]): Domains Or Every[WolfeError] =
    domains(Domains(varDoms: _*))(term)



  def domains(varDoms: Domains)(term: Term[Any]): Domains Or Every[WolfeError] = {
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
             elemDom <- elemDomain(s, ds))
          yield (ds ++ di) + (sa in elemDom)

      case fl@Foldl(seq, init, op) =>
        for (dseq <- dom(seq);
             dinit <- dom(init);
             elemDom <- seqDomain(seq,varDoms).map(_.elemDom);
             typedArgs = varDoms + (op.argument1 in dinit(init)) + (op.argument2 in elemDom);
             dop <- domains(typedArgs)(op)) yield (dseq ++ dinit ++ dop) + (fl in dinit(init))

      case ge@GetElement(p, e) =>
        for (dp <- dom(p);
             doms <- check(dp(p), TyperError(p, dp, "not a product domain")) { case ProductDom(d, _) => d })
          yield dp + (ge in doms(e))

      case tm@TensorMul(x1, x2) =>
        for (dx1 <- dom(x1);
             dx2 <- dom(x2);
             dims1 <- check(dx1(x1), TyperError(x1, dx1, "not a tensor domain")) { case TensorDom(d) => d };
             dims2 <- check(dx2(x2), TyperError(x2, dx2, "not a tensor domain")) { case TensorDom(d) => d };
             dims <- if (dims1.last == dims2.head)
               Good(dims1.dropRight(1) ::: dims2.drop(1))
             else
               Bad(One(TyperError(tm, dx1 ++ dx2, s"domains don't match: $dims1 * $dims2 in $tm"))))
          yield (dx1 ++ dx2) + (tm in TensorDom(dims))

      case lam@LambdaAbstraction2(a1,a2,body) =>
        for (da1 <- dom(a1);
             da2 <- dom(a2);
             dbody <- dom(body))
          yield (da1 ++ da2 ++ dbody) + (lam in FunDom2(da1(a1),da2(a2),dbody(body)))

      case d: DomainPreserving =>
        for (ds <- (d.parts map dom).combined) yield ds.reduce(_ ++ _) + (d in ds.head(d.parts.head))

    }
  }


  def elemDomain(s: Term[Seq[Any]], ds: Domains): Or[Dom[Any], One[TyperError]] = {
    check(ds(s), TyperError(s, ds, "not a sequence domain")) { case SeqDom(eD, _, _) => eD }
  }

  def seqDomain(s: Term[Seq[Any]], ds: Domains): Or[SeqDom[Any], One[TyperError]] = {
    check(ds(s), TyperError(s, ds, "not a sequence domain")) { case s:SeqDom[_] => s }
  }


  def main(args: Array[String]) {

    import Language._

    val i = Var[Int]("i")

    println(domains(Domains(i in RangeDom(0 until 3)))(i + i))
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

  def check[D <: Dom[Any]](term: Term[Any]): D Or One[WolfeError] = {
    try {
      Good(this (term).asInstanceOf[D])
    } catch {
      case c: ClassCastException => Bad(One(TyperError(term, this, "Expected different domain type")))
      case c: NoSuchElementException => Bad(One(TyperError(term, this, "domain not assigned")))
    }
  }

  override def toString = map.mkString("\n")
}

case class DomainBinding[+T](term: Term[T], dom: Dom[T])

object Domains {
  def apply(bindings: DomainBinding[Any]*) = {
    val r = new Domains
    r.map = bindings.map(b => b.term -> b.dom).toMap
    r
  }
}