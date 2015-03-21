package ml.wolfe.term

/**
 * @author riedel
 */
class Max(val obj: DoubleTerm, val wrt: Seq[Var[Dom]]) extends DoubleTerm {
  self =>
  val domain = Dom.doubles
  val vars = obj.vars.filterNot(wrt.contains)

  def isStatic = false

  val this2obj = VariableMapping(vars, obj.vars)
  val wrt2obj = VariableMapping(wrt, obj.vars)

  override def differentiatorImpl(diffWrt: Seq[AnyVar])(in: Settings, err: Setting, gradientAcc: Settings) =
    new AbstractDifferentiator(in, err, gradientAcc, diffWrt) {

      val eval = evaluatorImpl(in)

      val objObsVars = obj.vars filterNot diffWrt.contains
      val objInput = obj.createInputSettings()

      this2obj.linkTargetsToSource(in, objInput)
      wrt2obj.linkTargetsToSource(eval.argmaxer.result, objInput)

      val objGradient = obj.createZeroInputSettings()
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

  class MaxEvaluator(in: Settings) extends AbstractEvaluator(in) {

    val objInput = obj.createInputSettings()
    val argmaxer = obj.argmaxerImpl(wrt)(in, null)

    this2obj.linkTargetsToSource(in, objInput)
    wrt2obj.linkTargetsToSource(argmaxer.result, objInput)

    val objEval = obj.evaluatorImpl(objInput)

    def eval()(implicit execution: Execution) = {
      argmaxer.argmax()
      objEval.eval()

    }

    val output = objEval.output
  }


  override def evaluatorImpl(in: Settings) = new MaxEvaluator(in)

  def by(factory: ArgmaxerFactory) = {
    val newObj = new ProxyTerm[TypedDom[Double]] {
      def self = obj

      override def argmaxerImpl(wrt: Seq[Var[Dom]])(observed: Settings, msgs: Msgs) = {
        factory.argmaxer(obj, wrt)(observed, msgs)
      }

      def copy(args: IndexedSeq[ArgumentType]) = ???
    }
    new Max(newObj, wrt)
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

  def by(factory: ArgmaxerFactory) = {
    val newObj = new ProxyTerm[TypedDom[Double]] {
      def self = obj

      override def argmaxerImpl(wrt: Seq[Var[Dom]])(observed: Settings, msgs: Msgs) = {
        factory.argmaxer(obj, wrt)(observed, msgs)
      }

      def copy(args: IndexedSeq[ArgumentType]) = ???
    }
    new Argmax(newObj, wrt)
  }


}


