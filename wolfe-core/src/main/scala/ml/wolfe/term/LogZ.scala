package ml.wolfe.term

/**
 * @author riedel
 */
class LogZ(val obj: DoubleTerm, val wrt: Var[Dom]) extends DoubleTerm with Unary {
  self =>
  override def argument: ArgumentType = obj

  override def copy(arg: ArgumentType): Term[Dom] = new LogZ(arg, wrt)

  override type ArgumentType = DoubleTerm

  val domain = Dom.doubles
  val vars = obj.vars.filterNot(_ == wrt)

  //def isStatic = false

  val this2obj = VariableMapping(vars, obj.vars)
  val wrt2obj = VariableMapping(Seq(wrt), obj.vars)

  override def differentiatorImpl(diffWrt: Seq[AnyVar])(in: Settings, err: Setting, gradientAcc: Settings) =
    new AbstractDifferentiator(in, err, gradientAcc, diffWrt) {

      val eval = evaluatorImpl(in)

      val objObsVars = obj.vars filterNot diffWrt.contains
      val objInput = obj.createInputSettings()

      this2obj.linkTargetsToSource(in, objInput)
      wrt2obj.linkTargetsToSource(eval.argmaxer.result, objInput)

      val objGradient = obj.createSparseZeroInputSettings()
      this2obj.pairs foreach { case (src, tgt) => objGradient(tgt).setAdaptiveVectors(gradientAcc(src).adaptiveVectors) }

      val diff = obj.differentiatorImpl(diffWrt)(objInput, err, objGradient)

      def forward()(implicit execution: Execution) = {
        eval.eval()
      }

      def backward()(implicit execution: Execution) = {
        //eval was executed, and argmaxer.result is the maximizing assignment
        //we use this assignment as observation to the differentiator
        diff.gradientAccumulator foreach (_.resetToZero())
        diff.differentiate()
        //now the objective gradient holds a gradient for all objective vars, map this back to the max term vars
        this2obj.addBackward(gradientAcc, diff.gradientAccumulator)
      }

      val output = eval.output
    }

  class LogZEvaluator(in: Settings) extends AbstractEvaluator(in) {

    val objInput = obj.createInputSettings()
    val argmaxer = obj.argmaxerImpl(Seq(wrt))(in, null)

    this2obj.linkTargetsToSource(in, objInput)
    wrt2obj.linkTargetsToSource(argmaxer.result, objInput)

    val objEval = obj.evaluatorImpl(objInput)

    def eval()(implicit execution: Execution) = {
      //evaluate


      objEval.eval()

    }

    val output = objEval.output
  }

  val explicit = {
    //val terms = wrt.domain.map(v => wrt.domain.)
    //Sum()
  }


  override def evaluatorImpl(in: Settings) = new LogZEvaluator(in)



}




