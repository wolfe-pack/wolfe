package scalapplcodefest

/**
 * A predicate evaluates to a function. The function's behavior is defined
 * through a set of variables `pred(a)` and their assignments
 * in the state. In particular, the functions value at `a`
 * is the value assigned in the state to the variable `pred(a)`.
 *
 * @author Sebastian Riedel
 */
case class Predicate[A,B](name:Symbol, superDomain:Term[Set[A]],superRange:Term[Set[B]]) extends FunTerm[A, B] {

  thisPredicate =>

  def eval(state: State) = {
    for (d <- superDomain.eval(state); r <- superRange.eval(state)) yield new Fun[A, B] {
      def apply(a: A) = GroundAtom(thisPredicate, a).eval(state).get
      def isDefinedAt(a: A) = d(a) && GroundAtom(thisPredicate, a).eval(state).exists(r(_))
    }
  }
  def variables = AllGroundAtoms(thisPredicate).asInstanceOf[Set[Variable[Any]]]
  def domain[C >: Fun[A, B]] = Constant(Util.setToBeImplementedLater)
  def default = Fun{ case a => superRange.default.head }
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

  def domain[C >: B] = predicate.superRange.as[C]
}

/**
 * The set of all ground atom variables of the given predicate.
 * @param predicate the predicate of the ground atoms in this set.
 */
case class AllGroundAtoms[A,B](predicate:Predicate[A,B], condition:State = State.empty) extends Set[Variable[B]] {
  def contains(elem: Variable[B]) = elem match {
    case GroundAtom(p,_) => p == predicate
    case _ => false
  }
  def +(elem: Variable[B]) = SetUtil.Union(Set(this,Set(elem)))
  def -(elem: Variable[B]) = SetUtil.SetMinus(this,Set(elem))
  def iterator = predicate.superDomain.eval(condition) match {
    case Some(domain) => domain.iterator.map(arg => GroundAtom(predicate,arg))
    case _ => sys.error(s"The domain of $predicate is undefined and we can't iterate over its atoms" )
  }
}


