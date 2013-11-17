package scalapplcodefest

/**
 * A predicate evaluates to a function. The function's behavior is defined
 * through a set of variables `pred(a)` and their assignments
 * in the state. In particular, the functions value at `a`
 * is the value assigned in the state to the variable `pred(a)`.
 *
 * @author Sebastian Riedel
 */
case class Predicate[A,B](name:Symbol, funCandidateDom:Term[Set[A]],funRange:Term[Set[B]] = Constant(Bools)) extends FunTerm[A, B] {

  thisPredicate =>

  def eval(state: State) = {
    for (d <- funCandidateDom.eval(state); r <- funRange.eval(state)) yield new Fun[A, B] {
      def apply(a: A) = GroundAtom(thisPredicate, a).eval(state).get
      def isDefinedAt(a: A) = d(a) && GroundAtom(thisPredicate, a).eval(state).exists(r(_))
      def funCandidateDom = d
      def funRange = r
    }
  }
  def variables = AllGroundAtoms(thisPredicate).asInstanceOf[Set[Variable[Any]]]
  def domain[C >: Fun[A, B]] = Constant(Util.setToBeImplementedLater)
  def default = Fun{ case a => funRange.default.head }
  override def toString = name.toString()
}


/**
 * A GroundAtom is a variable that determines determines the function value of a predicate at
 * a particular argument.
 * @param predicate the predicate this ground atom refers to.
 * @param arg the argument of the atom.
 * @tparam A argument type.
 * @tparam B the type of the variable.
 */
case class GroundAtom[A, B](predicate: Predicate[A, B], arg: A) extends Variable[B] {

  import SetCastHelper._

  def domain[C >: B] = predicate.funRange.as[C]
  override def toString = s"${predicate.name.name}($arg)"
}

/**
 * The set of all ground atom variables of the given predicate.
 * @param predicate the predicate of the ground atoms in this set.
 * @param condition the state we use to evaluate the domain of the predicate with.
 */
case class AllGroundAtoms[A,B](predicate:Predicate[A,B], condition:State = State.empty) extends SetValue[Variable[Any]] {
  def contains(elem: Variable[Any]) = elem match {
    case GroundAtom(p,_) => p == predicate
    case _ => false
  }
  def iterator = predicate.funCandidateDom.eval(condition) match {
    case Some(domain) => domain.iterator.map(arg => GroundAtom(predicate,arg))
    case _ => sys.error(s"The domain of $predicate is undefined and we can't iterate over its atoms" )
  }
}

case class PartialGroundAtoms[A,B](predicate:Predicate[A,B],arg:Term[A], condition:State = State.empty)
  extends SetValue[Variable[Any]] {
  lazy val argDomain = arg.asInstanceOf[Term[Any]] match {
    case TupleTerm2(arg1,arg2) => (arg1.eval(condition),arg2.eval(condition)) match {
      case ((Some(a1),None)) => CartesianProduct2(Set(a1),arg2.domain.eval(condition).get)
      case ((None,Some(a2))) => CartesianProduct2(arg1.domain.eval(condition).get,Set(a2))
      case _ => arg.domain.eval(condition).get
    }
    case _ => arg.eval(condition).map(v => Set(v)).getOrElse(arg.domain.eval(condition).get)
  }
  lazy val argDomainCast = argDomain.asInstanceOf[Set[Any]]


  def contains(elem: Variable[Any]) = elem match {
    case GroundAtom(p,a) if p == predicate => argDomainCast(a)
    case _ => false
  }
  def iterator = argDomainCast.iterator.map(a => GroundAtom(predicate.asInstanceOf[Predicate[Any,B]],a))
}



/**
 * Set of all variables.
 */
case object AllVariables extends AllObjectsLarge[Variable[Any]]

