package scalapplcodefest.term


/**
 * A signature for lambda abstractions. A signature is a nested tuple of variables.
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

