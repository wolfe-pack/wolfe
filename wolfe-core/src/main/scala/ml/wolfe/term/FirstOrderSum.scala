package ml.wolfe.term

import ml.wolfe.Vect

/**
 * @author riedel
 */
case class FirstOrderDoubleSum[D <: Dom, Body <: DoubleTerm, R <: Term[VarSeqDom[D]]](range: R, variable: Var[D], body: Body)
  extends DoubleTerm with FirstOrderSum[D, Body, R] {
  val domain = body.domain

  def copy(args: IndexedSeq[ArgumentType]) =
    FirstOrderDoubleSum(args(0).asInstanceOf[Term[VarSeqDom[D]]],variable, args(1).asInstanceOf[DoubleTerm])

  def calculateSum(loop:Loop, eval: =>Unit, value: => Double)(implicit execution:Execution): Double = {
    var result = domain.zero
    loop(execution) {
      eval
      result += value
    }
    result
  }

  override def evaluatorImpl(in: Settings):FirstOrderSumEvaluator = new FirstOrderSumEvaluator(in) {
    def eval()(implicit execution:Execution) = {
      output.cont(0) = calculateSum(loop,bodyEval.eval(),bodyEval.output.cont(0))
    }
  }

  override def differentiatorImpl(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) = new FirstOrderSumDifferentiator(wrt)(in, err, gradientAcc) {
    def forward()(implicit execution:Execution):Unit = {
      output.cont(0) = calculateSum(loop,bodyDiff.forward(),bodyDiff.output.cont(0))
    }
  }

}




case class FirstOrderVectorSum[D <: Dom, Body <: VectorTerm, R <: Term[VarSeqDom[D]]](range: R, variable: Var[D], body: Body)
  extends VectorTerm with FirstOrderSum[D, Body, R] {
  val domain = body.domain

  def copy(args: IndexedSeq[ArgumentType]) =
    FirstOrderVectorSum(args(0).asInstanceOf[Term[VarSeqDom[D]]],variable, args(1).asInstanceOf[VectorTerm])

  def calculateSum(loop:Loop, eval: =>Unit, value: => Vect)(implicit execution:Execution): Vect = {
    var result = domain.zero
    loop(execution) {
      eval
      result += value
    }
    result
  }

  override def evaluatorImpl(in: Settings):FirstOrderSumEvaluator = new FirstOrderSumEvaluator(in) {
    def eval()(implicit execution:Execution) = {
      output.vect(0) = calculateSum(loop,bodyEval.eval(),bodyEval.output.vect(0))
    }
  }

  override def differentiatorImpl(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) = new FirstOrderSumDifferentiator(wrt)(in, err, gradientAcc) {
    def forward()(implicit execution:Execution):Unit = {
      output.vect(0) = calculateSum(loop,bodyDiff.forward(),bodyDiff.output.vect(0))
    }
  }

}






trait FirstOrderSum[D <: Dom, Body <: AnyTerm, R <: Term[VarSeqDom[D]]] extends NAry {
  sum =>

  def range: R
  def variable: Var[D]
  def body: Body

  val vars = (range.vars ++ body.vars).filterNot(_ == variable).distinct

  type ArgumentType = AnyTerm

  def arguments = IndexedSeq(range,body)

  //def isStatic = false //todo

  val this2body = VariableMapping(vars, body.vars)
  val this2range = VariableMapping(vars, range.vars)

  class Loop(termInputs: Settings) {
    val indexOfVar = body.vars.indexOf(variable)
    val varInput = variable.domain.createSetting()
    val bodyInput = body.createInputSettings()
    val rangeInput = range.createInputSettings()

    this2range.linkTargetsToSource(termInputs, rangeInput)
    this2body.linkTargetsToSource(termInputs, bodyInput)
    if (indexOfVar != -1) bodyInput(indexOfVar) = varInput

    //val bodyEval = body.evaluatorImpl(bodyInput)
    val rangeEval = range.evaluatorImpl(rangeInput)

    //val output = bodyEval.output

    def apply(execution:Execution)(procedure: => Unit): Unit = {
      rangeEval.eval()(execution)
      var result = 0.0
      val length = rangeEval.output.disc(0)
      val elemLength = range.domain.elementDom.lengths
      var offset = Offsets(discOff = 1)
      for (i <- 0 until length) {
        //copy element into body input at variable index
        if (indexOfVar != -1) varInput shallowAssign(rangeEval.output, offset, elemLength)
        //evaluate body
        //bodyEval.eval()(execution)
        //add to result
        procedure
        //move to next slot in range
        offset += elemLength
      }
    }

  }

  abstract class FirstOrderSumEvaluator(in:Settings) extends AbstractEvaluator(in)  {
    val output = domain.createSetting()
    val loop = new Loop(in)
    val bodyEval = body.evaluatorImpl(loop.bodyInput)
  }

  abstract class FirstOrderSumDifferentiator(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) extends AbstractDifferentiator(in,err,gradientAcc,wrt) {
      //val eval = evaluatorImpl(in)
      //val loop = new Loop(in)
      val loop = new Loop(in)
      val bodyDiff = body.differentiatorImpl(wrt)(loop.bodyInput, err, gradientAccumulator.linkedSettings(vars,body.vars))
      val output = domain.createSetting()

      def backward()(implicit execution:Execution) = {
        loop(execution) {
          bodyDiff.differentiate()
        }
      }

    }
}

