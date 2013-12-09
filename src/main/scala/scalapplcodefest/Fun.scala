package scalapplcodefest

/**
 * @author Sebastian Riedel
 */
/**
 * Partial functions with invariant argument and result types.
 * @tparam A argument type.
 * @tparam B return type.
 */
trait Fun[A, B] extends PartialFunction[A, B] {

  def funCandidateDom: Set[A]
  //TODO: would like to make this lazy
  def funDom: Set[A] = funCandidateDom.filter(isDefinedAt)
  def funRange: Set[B]


  override def toString() = getClass().getSimpleName
}

/**
 * This trait should be implemented by singleton function objects such as "Add" "And" etc. The trait provides
 * both a term version of the function and better support for pattern matching.
 * @tparam A argument type.
 * @tparam B return type.
 */
trait Operator[A, B] extends Fun[A, B] {
  self =>
  object Term extends Constant(this)

  def isDefinedAt(x: A) = true

  object Applied {
    def unapply(x: Term[Any]): Option[Term[A]] = x match {
      case FunApp(Constant(op), arg) if op == self => Some(arg.asInstanceOf[Term[A]])
      case _ => None
    }
  }

}

case class TypedFun[A,B](f:AnyFunction, funCandidateDom:Set[A],funRange:Set[B]) extends Fun[A,B] {
  val typed = f.asInstanceOf[PartialFunction[A,B]]
  def isDefinedAt(x: A) = typed.isDefinedAt(x)
  def apply(x:A) = typed(x)
}

/**
 * Helper object to build Fun objects.
 */
object Fun {
  def apply[A, B](f: PartialFunction[A, B], dom: Set[A] = new AllOfType[A], ran: Set[B] = new AllOfType[B]) = new Fun[A, B] {
    def apply(v1: A) = f.apply(v1)
    def isDefinedAt(x: A) = dom(x) && f.lift(x).exists(ran(_))
    def funCandidateDom = dom
    def funRange = ran
  }

  def empty[A, B] = new Fun[A, B] {
    def funCandidateDom = Set.empty
    def funRange = Set.empty
    def apply(v1: A) = sys.error(s"Empty function not defined at $v1")
    def isDefinedAt(x: A) = false
  }

  def table[A, B](domain: Set[A], f: PartialFunction[A, B]) = {
    val map = domain.map(a => a -> f(a)).toMap
    val range = map.map(_._2).toSet
    apply(map, domain, range)
  }

}

trait BinaryOperatorSameDomainAndRange[T] extends BinaryOperatorSameDomain[T, T] {
  self =>
  def funRange = dom

  def reduce(args: Term[Seq[T]]) = Reduce(Term, args)
  object Reduced {
    def unapply(x: Term[Any]): Option[Term[Seq[T]]] = x match {
      case Reduce(Constant(op), args) if op == self => Some(args.asInstanceOf[Term[Seq[T]]])
      case _ => None
    }
  }

}

trait BinaryOperatorSameDomain[T, R] extends BinaryOperator[T, T, R] {
  def dom: Set[T]
  def dom1 = dom
  def dom2 = dom
}

class Equals[T] extends BinaryOperatorSameDomain[T, Boolean] {
  def apply(v1: (T, T)) = v1._1 == v1._2
  def dom = new AllOfType[T]
  def funRange = Bools
  override def equals(p1: scala.Any) = p1 match {
    case e:Equals[_] => true
    case _ => false
  }
}

case object Equal extends BinaryOperatorSameDomain[Any, Boolean] {
  def funRange = Bools
  def dom = All
  def apply(x:(Any,Any)) = x._1 == x._2
}


trait BinaryOperator[T1, T2, R] extends Operator[(T1, T2), R] {

  self =>
  def dom1: Set[T1]
  def dom2: Set[T2]

  def funCandidateDom = CartesianProduct2(dom1, dom2)

  object Applied2 {
    def unapply(x: Term[Any]): Option[(Term[T1], Term[T2])] = x match {
      case FunApp(Constant(op), TupleTerm2(arg1, arg2)) if op == self =>
        Some((arg1.asInstanceOf[Term[T1]], arg2.asInstanceOf[Term[T2]]))
      case _ => None
    }
  }

}



