package ml.wolfe.term

/**
 * @author riedel
 */
class LogTerm[D <: Dom, T <: Term[D]](val toLog: T, val name:String = null) extends Term[D] with Unary {

  def nameToShow = if (name == null) toLog.toString else name


  type ArgumentType = T


  def argument = toLog

  def isStatic = toLog.isStatic

  override def toString = s"logged($toLog)"

  def copy(arg: ArgumentType) = new LogTerm[Dom,Term[Dom]](arg,name)

  override def evaluatorImpl(in: Settings) = new AbstractEvaluator(in) {
    val evalToLog = toLog.evaluatorImpl(in)

    def eval()(implicit execution: Execution) = {
      evalToLog.eval()
      if (execution.typ != Execution.Diff) {
//        val inputValues = for ((v, s) <- vars zip input) yield v.domain.toValue(s)
//        val inputString = if (inputValues.isEmpty) "[None]" else inputValues.mkString("\n -", "\n -", "")
        println("-----------------------------------")
        println(s"""Vars:       ${vars.mkString(",")}""")
        println(s"Input:      $input")
        println(s"Execution:  $execution")
        println(s"Term name:  $nameToShow")
        println(s"Term:       $toLog")

        //        println(s"Inputs:     $inputString")
        val outputValue = domain.toValue(output)
        println(s"Output:     $outputValue")
        toLog match {
          case composed: Composed[_] =>
            val composer = evalToLog.asInstanceOf[Composed[Dom]#ComposedEvaluator]
            val argValues = for ((a, s) <- composed.arguments zip composer.argOutputs) yield a.domain.toValue(s)
            val argString = argValues.mkString("\n -", "\n -", "")
            println(s"Arguments: $argString")
          case _ =>
        }
      }
    }

    val output: Setting = evalToLog.output
  }


  override def differentiatorImpl(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) =
    new AbstractDifferentiator(in, err, gradientAcc,wrt) {
      val diffToLog = toLog.differentiatorImpl(wrt)(in,err,gradientAcc)
      def forward()(implicit execution: Execution) = {
        diffToLog.forward()
//        val inputValues = for ((v, s) <- vars zip input) yield v.domain.toValue(s)
//        val inputString = if (inputValues.isEmpty) "[None]" else inputValues.mkString("\n -", "\n -", "")
        println("-----------------------------------")
        println(s"Execution:  $execution")
        println(s"Term name:  $nameToShow")
        println(s"Term:       $toLog")
//        println(s"Inputs:     $inputString")
        val outputValue = domain.toValue(output)
        println(s"Output:     $outputValue" )

      }

      def backward()(implicit execution: Execution) = {
        diffToLog.backward()
      }

      val output: Setting = diffToLog.output
    }

  val domain = toLog.domain

  def vars = toLog.vars


}
