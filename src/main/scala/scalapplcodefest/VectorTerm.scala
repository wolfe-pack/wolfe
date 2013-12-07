package scalapplcodefest

import org.scalautils.Good

/**
 * @author Sebastian Riedel
 */
trait VectorTerm extends Term[Vector] {
  def default = new SparseVector(0)
  def domain[C >: Vector] = Constant(Vector).asInstanceOf[Term[Set[C]]]
}

object VectorTerm {
  def apply(semantics:State => Vector, vars:Set[Variable[Any]] = Set.empty) =  new VectorTerm {
    def eval(state: State) = Good(semantics(state))
    def variables = vars
  }
}


