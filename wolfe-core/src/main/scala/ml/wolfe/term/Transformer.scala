package ml.wolfe.term

/**
 * @author riedel
 */
object Transformer {

  def propositionalizeVariables(term:Term[Dom]):Term[Dom] = term match {
    case c:NAry => c.copy(c.arguments.map(a => propositionalizeVariables(a).asInstanceOf[c.ArgumentType]))
    case v:Var[_] => ??? //todo: create a new variable for each atom in variable, needs memory of which variables have already been created
    case _ => term
  }

}
