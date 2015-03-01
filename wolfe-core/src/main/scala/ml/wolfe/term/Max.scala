package ml.wolfe.term

/**
 * @author riedel
 */
class Max(val obj: DoubleTerm, wrt: Seq[Var[Dom]]) extends DoubleTerm {
  self =>
  val domain = Dom.doubles
  val vars   = obj.vars.filterNot(wrt.contains)

  def atomsIterator = obj.atomsIterator.filter(a => vars.contains(a.ownerOrSelf))

  def evaluator() = new MaxEvaluator()

  def differentiator(wrt: Seq[Var[Dom]]) = {
    new Differentiator {

      val eval         = evaluator()
      val diff         = obj.differentiator(vars)
      val value        = domain.createSetting()
      val fullGradient = obj.vars.map(_.domain.createZeroSetting()).toArray
      val full2vars = VariableMapping(obj.vars,vars)
      //gradient is the gradient of the objective at the maximum

      def forwardProp(current: Array[Setting]) = {
        eval.eval(current, activation)
      }
      def term = self
      def withRespectTo = wrt
      def backProp(error: Setting, gradient: Array[Setting]) = {
        //we know that eval was executed, so we have the maximizing hidden assignment in eval.fullSetting
        //now get the gradient into fullGradient
        diff.addGradientAndValue(eval.fullSetting, error, fullGradient, value)
        //now filter full gradient to only contain the remaining free variables.
        full2vars.copyForwardDeep(fullGradient,gradient)
      }
    }
  }

  class MaxEvaluator extends Evaluator {
    val argmaxer      = obj.argmaxer(wrt)
    val objEval       = obj.evaluator()
    //val vars2obs = VariableMapping(vars,)
    val hiddenSetting = wrt.map(_.domain.createSetting()).toArray
    val fullSetting   = obj.vars.map(_.domain.createSetting()).toArray
    val inputs2full   = VariableMapping(vars, obj.vars)
    val hidden2full   = VariableMapping(wrt, obj.vars)
    def eval(inputs: Array[Setting], output: Setting) = {
      //get the argmax
      argmaxer.argmax(inputs, null, hiddenSetting)
      //copy into hiddenSetting into fullSetting
      hidden2full.copyForwardDeep(hiddenSetting, fullSetting)
      inputs2full.copyForwardDeep(inputs, fullSetting)
      //then evaluate score of argmax
      objEval.eval(fullSetting, output)
    }
  }

}

class Argmax[D <: Dom](val obj: DoubleTerm, val wrt:Var[D]) extends Term[D] {
  val domain = wrt.domain

  val vars = obj.vars.filter(_ != wrt)

  def atomsIterator = obj.atomsIterator.filter(_.ownerOrSelf != wrt)

  def evaluator() = new Evaluator {
    val maxer = obj.argmaxer(Seq(wrt))
    def eval(inputs: Array[Setting], output: Setting) = {
      maxer.argmax(inputs,null,Array(output))
    }
  }

  def differentiator(wrt: Seq[Var[Dom]]) = ???
}

class Argmax2[D <: Dom](val obj: DoubleTerm, val wrt:Var[D]) extends Term[D] {
  val domain = wrt.domain

  val vars = obj.vars.filter(_ != wrt)

  def atomsIterator = obj.atomsIterator.filter(_.ownerOrSelf != wrt)

  def evaluator() = new Evaluator {
    val maxer = obj.argmaxer(Seq(wrt))
    def eval(inputs: Array[Setting], output: Setting) = {
      maxer.argmax(inputs,null,Array(output))
    }
  }


  override def evaluatorImpl(in: Settings) = new AbstractEvaluator2(in) {
    val maxer = obj.argmaxerImpl(Seq(wrt))(in,null)
    def eval() = {
      maxer.argmax()
    }

    val output = maxer.result(0)
  }

  def differentiator(wrt: Seq[Var[Dom]]) = ???
}


