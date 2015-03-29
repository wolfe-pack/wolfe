package ml.wolfe.term


/**
 * @author riedel
 */
case class FirstOrderSum[D <: Dom, Body <: DoubleTerm, R <: Term[VarSeqDom[D]]](range: R, variable: Var[D], body: Body) extends DoubleTerm {
  sum =>
  val vars = (range.vars ++ body.vars).filterNot(_ == variable).distinct

  val domain = body.domain

  def isStatic = false //todo

  val this2body = VariableMapping(vars, body.vars)
  val this2range = VariableMapping(vars, range.vars)

  class Loop(termInputs: Settings) {
    val indexOfVar = body.vars.indexOf(variable)
    val varInput = variable.domain.createSetting()
    val bodyInput = body.createInputSettings()
    val rangeInput = range.createInputSettings()

    this2range.linkTargetsToSource(termInputs, rangeInput)
    this2body.linkTargetsToSource(termInputs, bodyInput)
    bodyInput(indexOfVar) = varInput

    val bodyEval = body.evaluatorImpl(bodyInput)
    val rangeEval = range.evaluatorImpl(rangeInput)

    val output = bodyEval.output

    def apply(execution:Execution)(procedure: => Unit): Unit = {
      rangeEval.eval()(execution)
      var result = 0.0
      val length = rangeEval.output.disc(0)
      val elemLength = range.domain.elementDom.lengths
      var offset = Offsets(discOff = 1)
      for (i <- 0 until length) {
        //copy element into body input at variable index
        varInput :=(rangeEval.output, offset, elemLength)
        //evaluate body
        bodyEval.eval()(execution)
        //add to result
        procedure
        //move to next slot in range
        offset += elemLength
      }
    }

  }

  override def evaluatorImpl(in: Settings) = new AbstractEvaluator(in)  {
    val output = domain.createSetting()
    val loop = new Loop(in)

    def eval()(implicit execution:Execution) = {
      var result = 0.0
      loop(execution) {
        result += loop.output.cont(0)
      }
      output.cont(0) = result

    }
  }

  override def differentiatorImpl(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) =
    new AbstractDifferentiator(in,err,gradientAcc,wrt) {

      val loop = new Loop(in)
      val eval = evaluatorImpl(in)
      val output = eval.output
      val bodyDiff = body.differentiatorImpl(wrt)(loop.bodyInput, err, gradientAccumulator.linkedSettings(vars,body.vars))

      def forward()(implicit execution:Execution) = {
        eval.eval()
      }

      def backward()(implicit execution:Execution) = {
        loop(execution) {
          bodyDiff.differentiate()
        }
      }

    }
}

