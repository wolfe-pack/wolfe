package ml.wolfe.term

/**
 * @author riedel
 */
object Transformation {


  /**
   * Shattering replaces structured variables with composed terms that construct structured
   * objects from atomic variables (of type [[ml.wolfe.term.Atom]]).
   * @param term the term to shatter.
   * @param domains the domain binding. This binding is necessary to infer how a variable should be further shattered.
   * @tparam T the type of the input and output term.
   * @return a term in which each variable has been replaced by a term that composes the value based on atomic variables
   *         of type [[ml.wolfe.term.Atom]]
   */
  def shatter[T](term: Term[T], domains: Domains) = {

    def shatterAtoms(term: Term[Any]): Term[Any] = term match {
      case Atom(v, head :: path, ProductDom(doms, constructor)) =>
        val product = head.asInstanceOf[Term[Product]]
        val args = doms.zipWithIndex.map { case (d, i) =>
          val next = GetElement(product, i)
          Atom(v, next :: head :: path, d)
        }
        val shattered = args map shatterAtoms
        ConstructProduct(shattered, constructor)
      case _ => term
    }

    val atomized = term.transform {
      case v: Var[_] => Atom(v, v :: Nil, domains(v))
    }

    val shattered = atomized.transform {
      case t => shatterAtoms(t)
    }

    shattered

  }
}

/**
 * A term that represents a sub-variable within a structured variable.
 * @param variable the root variable this sub-variable is a part of.
 * @param path the path from the root variable to this atom, with the leaf term first and the root variable last.
 * @param dom the domain of the atom.
 * @tparam T the value type of the atom.
 */
case class Atom[+T](variable: Var[Any], path: List[Term[Any]], dom: Dom[T]) extends Term[T]