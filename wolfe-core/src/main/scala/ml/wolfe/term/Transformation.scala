package ml.wolfe.term

/**
  * @author riedel
  */
object Transformation {

  /**
    * Groups variable access paths in a term tree into [[ml.wolfe.term.VarAccess]] objects.
    * @param term the term to group paths in.
    * @param filter a filter on the variables that are allowed to root access paths.
    * @tparam T value type of term.
    * @return a term with access paths replaced by [[ml.wolfe.term.VarAccess]] objects.
    */
  def induceAccessors[T](term: Term[T], filter: Var[Any] => Boolean = _ => true) = term.transform {
    case v: Var[_] if filter(v) => VarAccess(v, v :: Nil)
    case g@GetElement(VarAccess(v, path), _) => VarAccess(v, g :: path)
    case a@SeqApply(VarAccess(v, path), _) => VarAccess(v, a :: path)
  }

  /**
    * Shattering replaces structured variables with composed terms that construct structured
    * objects from atomic variables (of type [[ml.wolfe.term.VarAccess]]).
    * @param term the term to shatter.
    * @param domains the domain binding. This binding is necessary to infer how a variable should be further shattered.
    * @tparam T the type of the input and output term.
    * @return a term in which each variable has been replaced by a term that composes the value based on atomic variables
    *         of type [[ml.wolfe.term.VarAccess]]
    */
  def shatter[T](term: Term[T], domains: Domains) = {

    import ml.wolfe.Language._

    def shatterAtoms(term: Term[Any], domains: Domains): Term[Any] = {
      term -> domains(term) match {
        case (VarAccess(v, head :: path), ProductDom(doms, constructor)) =>
          val product = head.asInstanceOf[Term[Product]]
          val args = doms.zipWithIndex.map { case (d, i) =>
            val next = GetElement(product, i)
            VarAccess(v, next :: head :: path) -> d
          }
          val shattered = args map { case (a, d) => shatterAtoms(a, domains + (a in d)) }
          ConstructProduct(shattered, constructor)
        case _ => term
      }
    }

    val atomized = term.transform {
      case v: Var[_] => VarAccess(v, v :: Nil)
    }

    val shattered = atomized.transform {
      case a@VarAccess(v, _) => shatterAtoms(a, domains + (a in domains(v)))
    }

    shattered

  }

  /**
    * Simplifies Product(a1,a2,a3)(1) to a2.
    * @param term the term to collapse getters in.
    * @tparam T value type of term.
    * @return the term with occurrences of collapsable getters collapsed.
    */
  def collapseGetters[T](term: Term[T]) = term.transform {
    case GetElement(ConstructProduct(args, _), i) => args(i)
  }

}

/**
  * A term that represents a sub-variable within a structured variable.
  * @param variable the root variable this sub-variable is a part of.
  * @param path the path from the root variable to this atom, with the leaf term first and the root variable last.
  * @tparam T the value type of the atom.
  */
case class VarAccess[+T](variable: Var[Any], path: List[Accessor]) extends Term[T] {
  def isDynamic = path.collectFirst { case SeqApply(_, _) => true }.getOrElse(false)
}