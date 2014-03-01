package scalapplcodefest.legacy.value

import scalapplcodefest.AnyFunction

/**
 * @author Sebastian Riedel
 */
/**
 * Partial functions with invariant argument and result types. Knows its domain and range.
 * @tparam A argument type.
 * @tparam B return type.
 */
trait Fun[A, B] extends PartialFunction[A, B] {

  def funCandidateDom: Set[A]
  def funDom: Set[A] = funCandidateDom.filter(isDefinedAt)
  def funRange: Set[B]

  override def toString() = getClass.getSimpleName
}

/**
 * This trait should be implemented by singleton function objects such as "Add" "And" etc. The trait provides
 * both a term version of the function and better support for pattern matching.
 * @tparam A argument type.
 * @tparam B return type.
 */
trait Operator[A, B] extends Fun[A, B] {  //todo naming is not consistent with the DSL implicit
  self =>

  def isDefinedAt(x: A) = true

}

case class TypedFun[A,B](f:AnyFunction, funCandidateDom:Set[A],funRange:Set[B]) extends Fun[A,B] {
  val typed = f.asInstanceOf[PartialFunction[A,B]]
  def isDefinedAt(x: A) = typed.isDefinedAt(x)
  def apply(x:A) = typed(x)
}

trait DeepEqualFun {
  this:Fun[_,_] =>
  override def equals(p1: scala.Any) = p1 match {
    case f:Fun[_,_] =>
      f.funDom == funDom &&
        f.funDom.forall(a => f.asInstanceOf[Fun[Any,Any]](a) == this.asInstanceOf[Fun[Any,Any]](a))
    case _ => false
  }
}

/**
 * Helper object to build Fun objects.
 */
object Fun {
  def apply[A, B](f: PartialFunction[A, B], dom: Set[A] = new AllOfType[A], ran: Set[B] = new AllOfType[B]) =
    new Fun[A, B] with DeepEqualFun {
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

}

/**
 * Set of a functions from domain to range
 * @param domain the domain of the functions
 * @param range the range of the functions
 * @tparam A argument type.
 * @tparam B return type.
 */
case class AllFunctions[A, B](domain: Set[A], range: Set[B]) extends SetValue[Fun[A, B]] {

  self =>

  override def head = new Fun[A, B] with DeepEqualFun {
    def funCandidateDom = self.domain
    def funRange = range
    override def funDom = self.domain
    def apply(v1: A) = range.head
    def isDefinedAt(x: A) = domain(x)
  }

  def contains(elem: Fun[A, B]) = (elem.funDom == domain) && (elem.funRange == range)
  def iterator = {
    def allFunctions(d: List[A], r: List[B], funs: List[Fun[A, B]] = List(Fun.empty)): List[Fun[A, B]] = {
      d match {
        case Nil => funs
        case newArg :: tail =>
          val newFunctions = for (v <- r; f <- funs) yield new Fun[A, B] with DeepEqualFun {
            def funCandidateDom = self.domain
            def funRange = range
            override def funDom = self.domain
            def apply(v1: A) = if (v1 == newArg) v else f(v1)
            def isDefinedAt(x: A) = x == newArg || f.isDefinedAt(x)
          }
          allFunctions(tail, r, newFunctions)
      }
    }
    allFunctions(domain.toList, range.toList).iterator
  }
  override def equals(that: Any) = that match {
    case AllFunctions(d,r) => d == domain && r == range
    case _ => false
  }
}





