package scalapplcodefest.term

import scalapplcodefest.value.{Fun, CartesianProduct2, AllFunctions}
import scala.collection.mutable
import org.scalautils.Good
import scalapplcodefest.term.Sig.TypedBuilder

/**
 * A signature for lambda abstractions. A basic signature is a tuple of variables, and each variable
 * can appear in the body of the lambda abstraction.
 *
 * @tparam T type of values that the signature describes.
 *
 * @author Sebastian Riedel
 */
sealed trait Sig[T] extends Term[T] {
  def variables: Set[Variable[Any]]
  def toState(value: T): State
  def dom: Term[Set[T]]
  def domain[C >: T] = dom.asInstanceOf[Term[Set[C]]]
  def path(variable:Variable[Any]) = paths.get(variable)
  def allPaths(term:Term[_]) = Sig.allPaths(this,term)
  def newBuilder() = new TypedBuilder[T](Sig.builderFor(dom.value()))
  private val paths = Sig.var2Path(this)
}

case class VarSig[T](variable: Variable[T]) extends Sig[T] with Composite1[T, T] {
  def eval(state: State) = for (v1 <- variable.eval(state)) yield v1
  def default = variable.default
  def toState(value: T) = State(Map(variable -> value))
  def variables = Set(variable)
  def dom = variable.domain
  def components = variable
  def copy(t1: Term[T]) = VarSig(t1.asInstanceOf[Variable[T]])
  override def toString = variable.toString
}

case class TupleSig2[T1, T2](sig1: Sig[T1], sig2: Sig[T2]) extends Sig[(T1, T2)] with Composite2[T1, T2, (T1, T2)] {
  def eval(state: State) = for (v1 <- sig1.eval(state); v2 <- sig2.eval(state)) yield (v1, v2)
  def default = (sig1.default, sig2.default)
  def toState(value: (T1, T2)) = sig1.toState(value._1) + sig2.toState(value._2)
  def variables = sig1.variables ++ sig2.variables
  def dom = CartesianProductTerm2(sig1.dom, sig2.dom)
  def components = (sig1, sig2)
  def copy(t1: Term[T1], t2: Term[T2]) = TupleSig2(t1.asInstanceOf[Sig[T1]], t2.asInstanceOf[Sig[T2]])
  override def toString = s"($sig1,$sig2)"
}

case class TupleSig3[T1, T2, T3](sig1: Sig[T1], sig2: Sig[T2], sig3: Sig[T3])
  extends Sig[(T1, T2, T3)] with Composite3[T1, T2, T3, (T1, T2, T3)] {

  def eval(state: State) = for (v1 <- sig1.eval(state); v2 <- sig2.eval(state); v3 <- sig3.eval(state)) yield (v1, v2, v3)
  def default = (sig1.default, sig2.default, sig3.default)
  def toState(value: (T1, T2, T3)) = sig1.toState(value._1) + sig2.toState(value._2) + sig3.toState(value._3)
  def variables = sig1.variables ++ sig2.variables ++ sig3.variables
  def dom = CartesianProductTerm3(sig1.dom, sig2.dom, sig3.dom)
  def components = (sig1, sig2, sig3)
  def copy(t1: Term[T1], t2: Term[T2], t3: Term[T3]) = TupleSig3(t1.asInstanceOf[Sig[T1]], t2.asInstanceOf[Sig[T2]], t3.asInstanceOf[Sig[T3]])

  override def toString = s"($sig1,$sig2,$sig3)"
}

object Sig {
  def allPaths(sig: Sig[_], term: Term[_]): List[Path] = {
    val paths = term match {
      case v: Variable[_] =>
        val path = sig.path(v).get
        completePaths(v.domain.value(), List(path))
      case f@FunApp(v@Var(_, _), arg) => arg.eval() match {
        case Good(value) =>
          val FunTerm(_, range) = v
          val path = sig.path(v).get
          completePaths(range.value(), List(FunArgStep(value) :: path))
        case _ => allPaths(sig, v) ++ allPaths(sig, arg)
      }
      case c: Composite[_] => c.componentSeq.toList.flatMap(allPaths(sig, _))
      case _ => Nil
    }
    paths
  }

  def var2Path[_](sig: Sig[_]): Map[Variable[Any], Path] = {
    sig match {
      case VarSig(v) =>
        Map(v -> Nil)
      case TupleSig2(s1, s2) =>
        val m1 = var2Path(s1).mapValues(TupleArgStep(0) :: _)
        val m2 = var2Path(s2).mapValues(TupleArgStep(1) :: _)
        m1 ++ m2
      case TupleSig3(s1, s2, s3) =>
        val m1 = var2Path(s1).mapValues(TupleArgStep(0) :: _)
        val m2 = var2Path(s2).mapValues(TupleArgStep(1) :: _)
        val m3 = var2Path(s2).mapValues(TupleArgStep(2) :: _)
        m1 ++ m2 ++ m3
    }
  }

  def completePaths(domain: Set[_], paths: List[Path] = List(Nil)): List[Path] = {
    domain match {
      case AllFunctions(dom, range) =>
        val newPaths = for (arg <- dom.toList; oldPath <- paths) yield FunArgStep(arg) :: oldPath
        completePaths(range, newPaths)
      case CartesianProduct2(d1, d2) =>
        val newPaths = for (index <- (0 until 2).toList; oldPath <- paths) yield TupleArgStep(index) :: oldPath
        completePaths(d1, newPaths) ++ completePaths(d2, newPaths)
      case dom => paths
    }
  }

  type Path = List[PathStep]

  sealed trait PathStep

  case class TupleArgStep(index: Int) extends PathStep

  case class FunArgStep(arg: Any) extends PathStep

  def builderFor(dom: Set[_]): Builder = dom match {
    case AllFunctions(d, r) => new FunBuilder(d, r)
    case CartesianProduct2(d1, d2) => new TupleBuilder(Seq(d1, d2))
    case _ => new ValueBuilder(dom)
  }

  class TupleBuilder(domains: Seq[Set[_]]) extends Builder {
    val args = domains.map(builderFor)
    def build() = domains.size match {
      case 2 => (args(0).build, args(1).build)
      case 3 => (args(0).build, args(1).build, args(2).build)
    }
    def set(path: Path, value: Any) = path.head match {
      case TupleArgStep(index) => args(index).set(path.tail, value)
      case _ => sys.error("Bad path " + path)
    }
  }

  class FunBuilder(dom: Set[Any], range: Set[Any]) extends Builder {
    val map = new mutable.HashMap[Any, Builder]()
    for (arg <- dom) map(arg) = builderFor(range)
    def set(path: Path, value: Any) = {
      path.head match {
        case FunArgStep(arg) => map(arg).set(path.tail, value)
        case _ => sys.error("Bad path " + path)
      }
    }
    def build() = {
      Fun(map.map({case (k, b) => k -> b.build}).toMap, dom, range)
    }
  }

  class ValueBuilder(dom: Set[_]) extends Builder {
    var buffer: Any = dom.head
    def build() = buffer
    def set(path: Path, value: Any) = {
      if (path.isEmpty) buffer = value else sys.error("Bad path " + path)
    }
  }

  class TypedBuilder[T](builder:Builder) {
    def build() = builder.build().asInstanceOf[T]
    def set(path: Path, value: Any) { builder.set(path,value)}
  }

  trait Builder {
    def build(): Any
    def set(path: Path, value: Any)
  }

}