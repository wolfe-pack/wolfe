package scalapplcodefest

/**
 * @author Sebastian Riedel
 */
/**
 * Partial functions with invariant argument and result types. This is important for pattern matching.
 * @tparam A argument type.
 * @tparam B return type.
 */
trait Fun[A, B] extends PartialFunction[A, B] {

  self =>

  def funCandidateDom: Set[A]
  def funDom: Set[A] = funCandidateDom.filter(isDefinedAt)
  //TODO: would like to make this lazy
  def funRange: Set[B]

  object Term extends ConstantFun(this)
  object Applied {
    def unapply(x: Term[Any]): Option[Term[A]] = x match {
      case FunApp(ConstantFun(op), arg) if op == self => Some(arg.asInstanceOf[Term[A]])
      case _ => None
    }
  }

  override def toString() = getClass().getSimpleName
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

}

trait BinaryOperatorSameDomainAndRange[T] extends BinaryOperatorSameDomain[T,T] {
  self =>
  def funRange = dom

  object Reduced {
    def unapply(x: Term[Any]): Option[Term[Seq[T]]] = x match {
      case Reduce(ConstantFun(op), args) if op == self => Some(args.asInstanceOf[Term[Seq[T]]])
      case _ => None
    }
  }

}

trait BinaryOperatorSameDomain[T, R] extends BinaryOperator[T,T, R] {
  def dom: Set[T]
  def dom1 = dom
  def dom2 = dom
}

trait BinaryOperator[T1,T2, R] extends Fun[(T1, T2), R] {

  self =>
  def dom1: Set[T1]
  def dom2: Set[T2]

  def funCandidateDom = CartesianProduct2(dom1, dom2)
  def isDefinedAt(x: (T1, T2)) = true

  object Applied2 {
    def unapply(x: Term[Any]): Option[(Term[T1], Term[T2])] = x match {
      case FunApp(ConstantFun(op), TupleTerm2(arg1, arg2)) if op == self =>
        Some((arg1.asInstanceOf[Term[T1]], arg2.asInstanceOf[Term[T2]]))
      case _ => None
    }
  }

}



