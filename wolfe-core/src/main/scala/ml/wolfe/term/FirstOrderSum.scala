package ml.wolfe.term


/**
 * @author riedel
 */
class FirstOrderSum[D <: Dom, Body <: DoubleTerm, R <: Term[VarSeqDom[D]]](val range: R, val variable:Var[D], body:Body) extends DoubleTerm {
  sum =>
  def vars = (range.vars ++ body.vars).filterNot(_ == variable)

  def atomsIterator = (range.atomsIterator ++ body.atomsIterator).filterNot(_.ownerOrSelf == variable)

  val domain = body.domain

  trait SumProcessor {
    val indexOfVar = body.vars.indexOf(variable)
    val this2body = VariableMapping(vars,body.vars)
    val this2range = VariableMapping(vars,range.vars)

    val bodyInputs = body.createVariableSettings()
    val bodyOutput = body.domain.createSetting()
    val bodyEval = body.evaluator()

    val rangeInputs = range.createVariableSettings()
    val rangeOutput = range.domain.createSetting()
    val rangeEval = range.evaluator()

    def loop(inputs:Array[Setting],bodyInputs:Array[Setting] = bodyInputs)(procedure: => Unit): Unit = {
      this2body.copyForwardShallow(inputs,bodyInputs)
      this2range.copyForwardShallow(inputs,rangeInputs)
      rangeEval.eval(rangeInputs,rangeOutput)
      val length = rangeOutput.disc(0)
      val elemLength = range.domain.elementDom.lengths
      var offset = Offsets(discOff = 1)
      for (i <- 0 until length) {
        //copy element into body input at variable index
        rangeOutput.copyTo(bodyInputs(indexOfVar),offset,Offsets(),elemLength)
        procedure
        offset += elemLength
      }
    }

  }

  def evaluator() = new SumProcessor with Evaluator{


    def eval(inputs: Array[Setting], output: Setting) = {
      output.cont(0) = 0.0

      loop(inputs) {
        bodyEval.eval(bodyInputs,bodyOutput)
        output.cont(0) += bodyOutput.cont(0)
      }
    }
  }

  def differentiator(wrt: Seq[Var[Dom]]) = new SumProcessor with Differentiator{

    require(wrt.forall(!range.vars.contains(_)), "Cannot differentiate range/indices of sum")

    val eval = evaluator()
    var currentInputs:Array[Setting] = _

    def forwardProp(current: Array[Setting]) = {
      eval.eval(current,activation)
      currentInputs = current
    }

    def term = sum
    def withRespectTo = wrt

    val bodyDiff = body.differentiator(wrt)
    val bodyGradient = body.createVariableSettings()

    def backProp(error: Setting, gradient: Array[Setting]) = {
      this2body.copyForwardShallow(gradient,bodyGradient)
      loop(currentInputs) {
        bodyDiff.addGradientAndValue(bodyInputs,error,bodyGradient,bodyOutput)
      }
      this2body.copyBackwardShallow(gradient,bodyGradient)
    }
  }

  val this2body = VariableMapping(vars,body.vars)
  val this2range = VariableMapping(vars,range.vars)

  override def evaluator2(in: Settings) = new Evaluator2 {
    val input = in
    val output = domain.createSetting()

    val indexOfVar = body.vars.indexOf(variable)
    val varInput = variable.domain.createSetting()
    val bodyInput = body.createSettings()
    val rangeInput = range.createSettings()

    this2range.linkTargetsToSource(in,rangeInput)
    this2body.linkTargetsToSource(in,bodyInput)
    bodyInput(indexOfVar) = varInput

    val bodyEval = body.evaluator2(bodyInput)
    val rangeEval = range.evaluator2(rangeInput)

    def eval() = {
      rangeEval.eval()
      var result = 0.0
      val length = rangeEval.output.disc(0)
      val elemLength = range.domain.elementDom.lengths
      var offset = Offsets(discOff = 1)
      for (i <- 0 until length) {
        //copy element into body input at variable index
        varInput := (rangeEval.output,offset,elemLength)
        //evaluate body
        bodyEval.eval()
        //add to result
        result += bodyEval.output.cont(0)
        //move to next slot in range
        offset += elemLength
      }
      output.cont(0) = result

    }
  }
}

