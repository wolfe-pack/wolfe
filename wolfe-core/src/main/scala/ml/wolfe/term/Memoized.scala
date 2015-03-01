package ml.wolfe.term

/**
 * Each evaluator
 * @author riedel
 */
class Memoized[D <: Dom, T <: Term[D]](term:T) extends Term[D] {
  val domain = term.domain
  def vars = term.vars

  override def evaluatorImpl(in: Settings) = new AbstractEvaluator2(in) {
    def eval()(implicit execution:Execution) = {

    }

    val output: Setting = ???
  }

  def evaluator() = ???

  def atomsIterator = ???

  def differentiator(wrt: Seq[Var[Dom]]) = ???
}
