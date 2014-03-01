package ml.wolfe.legacy.term

import org.scalautils.Good
import ml.wolfe._

/**
 * @author Sebastian Riedel
 */
trait VectorTerm extends Term[FactorieVector] {
  def default = new SparseVector(0)
  def domain[C >: FactorieVector] = Constant(Vector).asInstanceOf[Term[Set[C]]]
}

object VectorTerm {
  def apply(semantics:State => FactorieVector, vars:Set[Variable[Any]] = Set.empty) =  new VectorTerm {
    def eval(state: State) = Good(semantics(state))
    def variables = vars
  }
}


