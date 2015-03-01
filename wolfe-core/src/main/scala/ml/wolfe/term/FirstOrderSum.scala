package ml.wolfe.term


/**
 * @author riedel
 */
class FirstOrderSum[D <: Dom, Body <: DoubleTerm, R <: Term[VarSeqDom[D]]](val range: R, val variable: Var[D], body: Body) extends DoubleTerm {
  sum =>
  def vars = (range.vars ++ body.vars).filterNot(_ == variable)

  def atomsIterator = (range.atomsIterator ++ body.atomsIterator).filterNot(_.ownerOrSelf == variable)

  val domain = body.domain

  trait SumProcessor {
    val indexOfVar = body.vars.indexOf(variable)
    val this2body = VariableMapping(vars, body.vars)
    val this2range = VariableMapping(vars, range.vars)

    val bodyInputs = body.createVariableSettings()
    val bodyOutput = body.domain.createSetting()
    val bodyEval = body.evaluator()

    val rangeInputs = range.createVariableSettings()
    val rangeOutput = range.domain.createSetting()
    val rangeEval = range.evaluator()

    def loop(inputs: Array[Setting], bodyInputs: Array[Setting] = bodyInputs)(procedure: => Unit): Unit = {
      this2body.copyForwardShallow(inputs, bodyInputs)
      this2range.copyForwardShallow(inputs, rangeInputs)
      rangeEval.eval(rangeInputs, rangeOutput)
      val length = rangeOutput.disc(0)
      val elemLength = range.domain.elementDom.lengths
      var offset = Offsets(discOff = 1)
      for (i <- 0 until length) {
        //copy element into body input at variable index
        rangeOutput.copyTo(bodyInputs(indexOfVar), offset, Offsets(), elemLength)
        procedure
        offset += elemLength
      }
    }

  }

  def evaluator() = new SumProcessor with Evaluator {


    def eval(inputs: Array[Setting], output: Setting) = {
      output.cont(0) = 0.0

      loop(inputs) {
        bodyEval.eval(bodyInputs, bodyOutput)
        output.cont(0) += bodyOutput.cont(0)
      }
    }
  }

  def differentiator(wrt: Seq[Var[Dom]]) = new SumProcessor with Differentiator {

    require(wrt.forall(!range.vars.contains(_)), "Cannot differentiate range/indices of sum")

    val eval = evaluator()
    var currentInputs: Array[Setting] = _

    def forwardProp(current: Array[Setting]) = {
      eval.eval(current, activation)
      currentInputs = current
    }

    def term = sum

    def withRespectTo = wrt

    val bodyDiff = body.differentiator(wrt)
    val bodyGradient = body.createVariableSettings()

    def backProp(error: Setting, gradient: Array[Setting]) = {
      this2body.copyForwardShallow(gradient, bodyGradient)
      loop(currentInputs) {
        bodyDiff.addGradientAndValue(bodyInputs, error, bodyGradient, bodyOutput)
      }
      this2body.copyBackwardShallow(gradient, bodyGradient)
    }
  }

  val this2body = VariableMapping(vars, body.vars)
  val this2range = VariableMapping(vars, range.vars)

  class Loop(termInputs: Settings) {
    val indexOfVar = body.vars.indexOf(variable)
    val varInput = variable.domain.createSetting()
    val bodyInput = body.createSettings()
    val rangeInput = range.createSettings()

    this2range.linkTargetsToSource(termInputs, rangeInput)
    this2body.linkTargetsToSource(termInputs, bodyInput)
    bodyInput(indexOfVar) = varInput

    val bodyEval = body.evaluatorImpl(bodyInput)
    val rangeEval = range.evaluatorImpl(rangeInput)

    val output = bodyEval.output

    def apply(procedure: => Unit): Unit = {
      rangeEval.eval()
      var result = 0.0
      val length = rangeEval.output.disc(0)
      val elemLength = range.domain.elementDom.lengths
      var offset = Offsets(discOff = 1)
      for (i <- 0 until length) {
        //copy element into body input at variable index
        varInput :=(rangeEval.output, offset, elemLength)
        //evaluate body
        bodyEval.eval()
        //add to result
        procedure
        //move to next slot in range
        offset += elemLength
      }
    }

  }

  override def evaluatorImpl(in: Settings) = new AbstractEvaluator2(in)  {
    val output = domain.createSetting()
    val loop = new Loop(in)

    def eval() = {
      var result = 0.0
      loop {
        result += loop.output.cont(0)
      }
      output.cont(0) = result

    }
  }

  override def differentiator2(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) =
    new AbstractDifferentiator2(in,err,gradientAcc) {

      val loop = new Loop(in)
      val eval = evaluatorImpl(in)
      val output = eval.output
      val bodyDiff = body.differentiator2(wrt)(loop.bodyInput, err, gradientAccumulator.linkedSettings(vars,body.vars))

      def forward() = {
        eval.eval()
      }

      def backward() = {
        loop {
          bodyDiff.differentiate()
        }
      }

    }
}

