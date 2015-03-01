package ml.wolfe.term

/**
 * Each evaluator
 * @author riedel
 */
class Memoized[D <: Dom, T <: Term[D]](term:T) extends Term[D] {
  val domain:term.domain.type = term.domain
  def vars = term.vars

  private val cachedOut = domain.createSetting()
  private val inputToUniqueEval = createSettings()
  private val uniqueEval = term.evaluatorImpl(inputToUniqueEval)
  private var currentExecution: Execution = null

  override def evaluatorImpl(in: Settings) = new AbstractEvaluator2(in) {

    def eval()(implicit execution:Execution) = {
      if (execution != currentExecution) {
        inputToUniqueEval := input
        currentExecution = execution
        uniqueEval.eval()
      }
    }

    val output: Setting = uniqueEval.output
  }

  def evaluator() = ???

  def atomsIterator = ???

  def differentiator(wrt: Seq[Var[Dom]]) = ???
}
