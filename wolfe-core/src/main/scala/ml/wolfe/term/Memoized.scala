package ml.wolfe.term

import scala.collection.mutable

/**
 * Each evaluator
 * @author riedel
 */
class Memoized[D <: Dom, T <: Term[D]](val term:T) extends Term[D] with NAry {
  val domain:term.domain.type = term.domain
  def vars = term.vars

  private val inputToUniqueEval = createInputSettings()
  private val uniqueEval = term.evaluatorImpl(inputToUniqueEval)
  private val uniqueDiffs = new mutable.HashMap[Seq[Var[Dom]],Differentiator]()
  private var currentExecution: Execution = null
  private val inputs2ValueInCurrentExecution = new mutable.HashMap[Any,CurrentSettingForInput]


  type ArgumentType = T


  def arguments = mutable.IndexedSeq(term)


  def copy(args: IndexedSeq[ArgumentType]) = new Memoized[D,T](args(0))

  class CurrentSettingForInput(val inputValue:Any) {

    val output = domain.createSetting()
    var needsUpdate = true

    def updateIfNeeded(body: => Setting): Unit = {
      if (needsUpdate) {
        output := body
        needsUpdate = false
      }
    }

    def clear(): Unit = {
      needsUpdate = true
    }

  }

  def isStatic = term.isStatic

  override def evaluatorImpl(in: Settings) = new AbstractEvaluator(in) {
    private val cachedOut = domain.createSetting()

    def eval()(implicit execution:Execution) = {
      val inputValue = if (input.length == 0) Nil else (input zip vars) map {case(i,v) => v.domain.toValue(i)}
      if (currentExecution == null || execution.num != currentExecution.num) {
        inputs2ValueInCurrentExecution.values foreach (_.clear())
        currentExecution = execution
      }
      val outputSetting = inputs2ValueInCurrentExecution.getOrElseUpdate(inputValue, new CurrentSettingForInput(inputValue))
      outputSetting.updateIfNeeded {
        inputToUniqueEval := input
        uniqueEval.eval()
        uniqueEval.output
      }
      cachedOut := outputSetting.output
    }

    val output: Setting = cachedOut
  }

  override def differentiatorImpl(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) =
    new AbstractDifferentiator(in,err,gradientAcc,wrt) {
      val diff = uniqueDiffs.getOrElseUpdate(wrt, {
        val uniqueInputs = createInputSettings()
        val uniqueErr = domain.createSetting()
        val uniqueGradient = createInputSettings()
        new ProxyDifferentiator(term.differentiatorImpl(wrt)(uniqueInputs,uniqueErr,uniqueGradient))
      })
      val cachedOut = domain.createSetting()

      def forward()(implicit execution: Execution) = {
        val inputValue = if (input.length == 0) Nil else (input zip vars) map {case(i,v) => v.domain.toValue(i)}
        if (currentExecution == null || execution.num != currentExecution.num) {
          inputs2ValueInCurrentExecution.values foreach (_.clear())
          currentExecution = execution
        }
        val outputSetting = inputs2ValueInCurrentExecution.getOrElseUpdate(inputValue, new CurrentSettingForInput(inputValue))
        outputSetting.updateIfNeeded {
          diff.input := input
          diff.forward()
          diff.output
        }
        cachedOut := outputSetting.output
      }

      def backward()(implicit execution: Execution) = {
        diff.backward()
      }

      val output: Setting = cachedOut
    }

  override def toString = s"mem($term)"
}
