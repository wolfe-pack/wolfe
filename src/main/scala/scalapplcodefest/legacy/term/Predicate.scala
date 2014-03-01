package scalapplcodefest.legacy.term

import org.scalautils.Good
import scalapplcodefest.legacy.value._
import scalapplcodefest.legacy._
import org.scalautils.Bad

/**
 * A predicate evaluates to a function. The function's behavior is defined
 * through a set of variables `pred(a)` and their assignments
 * in the state. In particular, the functions value at `a`
 * is the value assigned in the state to the variable `pred(a)`.
 *
 * @author Sebastian Riedel
 */
case class Predicate[A,B](name:Symbol, funCandidateDom:Term[Set[A]],funRange:Term[Set[B]] = Constant(Bools))
  extends FunTerm[A, B] with Composite2[Set[A],Set[B],Fun[A,B]] {

  thisPredicate =>

  def eval(state: State) = {
    for (d <- funCandidateDom.eval(state); r <- funRange.eval(state)) yield new Fun[A, B] {
      def apply(a: A) = GroundAtom(thisPredicate, a).eval(state).get
      def isDefinedAt(a: A) = d(a) && GroundAtom(thisPredicate, a).eval(state).exists(r(_))
      def funCandidateDom = d
      def funRange = r
    }
  }
  override def variables = AllGroundAtoms(thisPredicate).asInstanceOf[Set[Variable[Any]]]
  def domain[C >: Fun[A, B]] = TermDSL.all[C]
  def default = Fun{ case a => funRange.default.head }
  override def toString = name.toString()
  override def equals(p1: scala.Any) = p1 match {
    case Predicate(n,_,_) => n == name
    case _ => false
  }
  override def hashCode() = name.hashCode()
  def components = (funCandidateDom,funRange)
  def copy(t1: Term[Set[A]], t2: Term[Set[B]]) = Predicate(name,t1,t2)

}


/**
 * A GroundAtom is a variable that determines determines the function value of a predicate at
 * a particular argument.
 * @param predicate the predicate this ground atom refers to.
 * @param arg the argument of the atom.
 * @tparam A argument type.
 * @tparam B the type of the variable.
 */
case class GroundAtom[A, B](predicate: Predicate[A, B], arg: A) extends Variable[B] with Composite1[Fun[A,B],B] {

  import SetCastHelper._

  def domain[C >: B] = predicate.funRange.as[C]
  override def toString = s"${predicate.name.name}($arg)"
  def components = predicate
  def copy(t1: Term[Fun[A, B]]) = GroundAtom(t1.asInstanceOf[Predicate[A,B]],arg)
  override def variables = super[Variable].variables
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
    case Good(domain) => domain.iterator.map(arg => GroundAtom(predicate,arg))
    case Bad(undefined) => sys.error(s"$predicate is undefined and we can't iterate over its atoms because of $undefined" )
  }
}

case class PartialGroundAtoms[A,B](predicate:Predicate[A,B],arg:Term[A], condition:State = State.empty)
  extends SetValue[Variable[Any]] {
  lazy val argDomain = arg.asInstanceOf[Term[Any]] match {
    case TupleTerm2(arg1,arg2) => (arg1.eval(condition),arg2.eval(condition)) match {
      case ((Good(a1),Bad(_))) => CartesianProduct2(Set(a1),arg2.domain.eval(condition).get)
      case ((Bad(_),Good(a2))) => CartesianProduct2(arg1.domain.eval(condition).get,Set(a2))
      case _ => arg.domain.eval(condition).get
    }
    case _ => arg.eval(condition).map(v => Set(v)).getOrElse({
      predicate.funCandidateDom.eval(condition).get
    })
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

