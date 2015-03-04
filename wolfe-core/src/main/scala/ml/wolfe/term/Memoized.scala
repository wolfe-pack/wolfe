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
  private val inputs2ValueInCurrentExecution = new mutable.HashMap[Any,Setting]

  override def evaluatorImpl(in: Settings) = new AbstractEvaluator2(in) {

    def eval()(implicit execution:Execution) = {
      val inputValue = (input zip vars) map {case(i,v) => v.domain.toValue(i)}
      if (execution != currentExecution) {
        inputs2ValueInCurrentExecution.clear()
        currentExecution = execution
      }
      val outputSetting = inputs2ValueInCurrentExecution.getOrElseUpdate(inputValue, {
        inputToUniqueEval := input
        uniqueEval.eval()
        val setting = domain.createSetting()
        setting := uniqueEval.output
        setting
      })
      cachedOut := outputSetting
    }

    val output: Setting = cachedOut
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
      val cachedOut = domain.createSetting()

      def forward()(implicit execution: Execution) = {
        val inputValue = (input zip vars) map {case(i,v) => v.domain.toValue(i)}
        if (execution != currentExecution) {
          inputs2ValueInCurrentExecution.clear()
          currentExecution = execution
        }
        val outputSetting = inputs2ValueInCurrentExecution.getOrElseUpdate(inputValue, {
          diff.input := input
          diff.forward()
          val setting = domain.createSetting()
          setting := diff.output
          setting
        })
        cachedOut := outputSetting
      }

      def backward()(implicit execution: Execution) = {
        diff.backward()
      }

      val output: Setting = cachedOut
    }

  def evaluator() = ???

  def atomsIterator = ???

  def differentiator(wrt: Seq[Var[Dom]]) = ???

  override def toString = s"mem($term)"
}
