package ml.wolfe.term

/**
 * @author riedel
 */
class Max(val obj: DoubleTerm, wrt: Seq[Var[Dom]]) extends DoubleTerm {
  self =>
  val domain = Dom.doubles
  val vars = obj.vars.filterNot(wrt.contains)

  def isStatic = false

  val this2obj = VariableMapping(vars,obj.vars)
  val wrt2obj = VariableMapping(wrt,obj.vars)

  //  def differentiatorOld(wrt: Seq[Var[Dom]]) = {
  //    new DifferentiatorOld {
  //
  //      val eval         = evaluatorOld()
  //      val diff         = obj.differentiatorOld(vars)
  //      val value        = domain.createSetting()
  //      val fullGradient = obj.vars.map(_.domain.createZeroSetting()).toArray
  //      val full2vars = VariableMapping(obj.vars,vars)
  //      //gradient is the gradient of the objective at the maximum
  //
  //      def forwardProp(current: Array[Setting]) = {
  //        eval.eval(current, activation)
  //      }
  //      def term = self
  //      def withRespectTo = wrt
  //      def backProp(error: Setting, gradient: Array[Setting]) = {
  //        //we know that eval was executed, so we have the maximizing hidden assignment in eval.fullSetting
  //        //now get the gradient into fullGradient
  //        diff.addGradientAndValue(eval.fullSetting, error, fullGradient, value)
  //        //now filter full gradient to only contain the remaining free variables.
  //        full2vars.copyForwardDeep(fullGradient,gradient)
  //      }
  //    }
  //  }

  //  class MaxEvaluatorOld extends EvaluatorOld {
  //    val argmaxer      = obj.argmaxerOld(wrt)
  //    val objEval       = obj.evaluatorOld()
  //    //val vars2obs = VariableMapping(vars,)
  //    val hiddenSetting = wrt.map(_.domain.createSetting()).toArray
  //    val fullSetting   = obj.vars.map(_.domain.createSetting()).toArray
  //    val inputs2full   = VariableMapping(vars, obj.vars)
  //    val hidden2full   = VariableMapping(wrt, obj.vars)
  //    def eval(inputs: Array[Setting], output: Setting) = {
  //      //get the argmax
  //      argmaxer.argmax(inputs, null, hiddenSetting)
  //      //copy into hiddenSetting into fullSetting
  //      hidden2full.copyForwardDeep(hiddenSetting, fullSetting)
  //      inputs2full.copyForwardDeep(inputs, fullSetting)
  //      //then evaluate score of argmax
  //      objEval.eval(fullSetting, output)
  //    }
  //  }
  override def evaluatorImpl(in: Settings) = new AbstractEvaluator(in) {

    val objInput = obj.createInputSettings()
    val argmaxer = obj.argmaxerImpl(wrt)(in,null)
    val objEval = obj.evaluatorImpl(objInput)

    this2obj.linkTargetsToSource(in,objInput)
    wrt2obj.linkTargetsToSource(argmaxer.result,objInput)

    def eval()(implicit execution: Execution) = {
      argmaxer.argmax()
      objEval.eval()

    }

    val output = objEval.output
  }
}

class Argmax[D <: Dom](val obj: DoubleTerm, val wrt: Var[D]) extends Term[D] {
  val domain = wrt.domain

  val vars = obj.vars.filter(_ != wrt)

  def isStatic = false

  override def evaluatorImpl(in: Settings) = new AbstractEvaluator(in) {
    val maxer = obj.argmaxerImpl(Seq(wrt))(in, null)

    def eval()(implicit execution: Execution) = {
      maxer.argmax()
    }

    val output = maxer.result(0)
  }


}


