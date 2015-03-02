package ml.wolfe.term

import scala.collection.mutable

/**
 * Each evaluator
 * @author riedel
 */
class Memoized[D <: Dom, T <: Term[D]](val term:T) extends Term[D] {
  val domain:term.domain.type = term.domain
  def vars = term.vars

  private val cachedOut = domain.createSetting()
  private val inputToUniqueEval = createSettings()
  private val uniqueEval = term.evaluatorImpl(inputToUniqueEval)
  private val uniqueDiffs = new mutable.HashMap[Seq[Var[Dom]],Differentiator2]()
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

  //todo: this should be memoized too (what does this mean?)
  override def differentiator2(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) =
    new AbstractDifferentiator2(in,err,gradientAcc) {
      val diff = uniqueDiffs.getOrElseUpdate(wrt, {
        val uniqueInputs = createSettings()
        val uniqueErr = domain.createSetting()
        val uniqueGradient = createSettings()
        term.differentiator2(wrt)(uniqueInputs,uniqueErr,uniqueGradient)
      })

      def forward()(implicit execution: Execution) = {
        if (execution != currentExecution) {
          diff.input := input
          diff.forward()
          currentExecution = execution
        }
      }

      def backward()(implicit execution: Execution) = {
        diff.backward()
      }

      val output: Setting = diff.output
    }

  def evaluator() = ???

  def atomsIterator = ???

  def differentiator(wrt: Seq[Var[Dom]]) = ???
}
