package ml.wolfe.term

/**
 * @author riedel
 */
trait TermMap[A <: Term[Dom], D <: Dom] extends Term[D] {
  val term:A
  val domain:D
  def f(arg:term.domain.Value):domain.Value

  def vars = term.vars


  override def evaluatorImpl(in: Settings) = new AbstractEvaluator(in) {
    val termEval = term.evaluatorImpl(in)
    var currentExecution:Execution = null
    def eval()(implicit execution: Execution): Unit = {
      if (currentExecution != execution) {
        termEval.eval()
        val value = term.domain.toValue(termEval.output)
        val mapped = f(value)
        domain.copyValue(mapped, output)
        currentExecution = execution
      }
    }
    val output = domain.createSetting()
  }

  def differentiatorOld(wrt: Seq[Var[Dom]]) = ???
}

trait TermFlatMap[A <: Term[Dom], D <: Dom] extends Term[D] {
  val term:A
  val domain:D
  def f(arg:term.domain.Value):Term[D]

  lazy val prototype = f(term.domain.zero)

  lazy val this2proto = VariableMapping(vars,prototype.vars)


  def vars = (term.vars ++ prototype.vars).distinct

  override def evaluatorImpl(in: Settings) = new AbstractEvaluator(in) {
    val termEval = term.evaluatorImpl(in)
    var currentExecution:Execution = null
    def eval()(implicit execution: Execution): Unit = {
      if (currentExecution != execution) {
        termEval.eval()
        val value = term.domain.toValue(termEval.output)
        val mapped = f(value)
        val mappedEval = mapped.evaluatorImpl(input.linkedSettings(vars,mapped.vars))
        mappedEval.eval()
        output := mappedEval.output
      }
    }
    val output = domain.createSetting()
  }

}

