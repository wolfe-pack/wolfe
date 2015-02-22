package ml.wolfe.term

import ml.wolfe.term.ExhaustiveSearch.AllSettings

/**
 * @author riedel
 */
class FirstOrderSum[D <: Dom, Body <: DoubleTerm](val variable:Var[D], body:Body) extends DoubleTerm {
  sum =>
  def vars = body.vars.filterNot(_ == variable)

  def atomsIterator = body.atomsIterator.filterNot(_.ownerOrSelf == variable)

  val domain = body.domain

  trait SumProcessor {
    val indexOfVar = body.vars.indexOf(variable)
    val this2body = VariableMapping(vars,body.vars)
    val bodyInputs = body.createVariableSettings()
    val atomsToIterate = variable.atoms.disc.map(a => ExhaustiveSearch.IndexedAtom(a,indexOfVar)).toIndexedSeq
    val iterator = new AllSettings(atomsToIterate)
    val bodyOutput = body.domain.createSetting()
    val bodyEval = body.evaluator()
  }

  def evaluator() = new SumProcessor with Evaluator{
    def eval(inputs: Array[Setting], output: Setting) = {
      output.cont(0) = 0.0
      this2body.copyForwardShallow(inputs,bodyInputs)
      //need to be able to iterate over domain
      iterator.iterate(bodyInputs) {
        bodyEval.eval(bodyInputs,bodyOutput)
        output.cont(0) += bodyOutput.cont(0)
      }
    }
  }

  def differentiator(wrt: Seq[Var[Dom]]) = new SumProcessor with Differentiator{
    val eval = evaluator()

    def forwardProp(current: Array[Setting]) = {
      eval.eval(current,activation)
      this2body.copyForwardShallow(current,bodyInputs)
    }

    def term = sum
    def withRespectTo = wrt

    val bodyDiff = body.differentiator(wrt)
    val bodyGradient = body.createVariableSettings()

    def backProp(error: Setting, gradient: Array[Setting]) = {
      this2body.copyForwardShallow(gradient,bodyGradient)
      iterator.iterate(bodyInputs) {
        bodyDiff.addGradientAndValue(bodyInputs,error,bodyGradient,bodyOutput)
      }
      this2body.copyBackwardShallow(gradient,bodyGradient)
    }
  }
}

/**
 * @author riedel
 */
class FirstOrderSum2[D <: Dom, Body <: DoubleTerm, R <: Term[SeqDom[D]]](val range: R, val variable:Var[D], body:Body) extends DoubleTerm {
  sum =>
  def vars = (range.vars ++ body.vars).filterNot(_ == variable)

  def atomsIterator = (range.atomsIterator ++ body.atomsIterator).filterNot(_.ownerOrSelf == variable)

  val domain = body.domain

  trait SumProcessor {
    val indexOfVar = body.vars.indexOf(variable)
    val this2body = VariableMapping(vars,body.vars)
    val bodyInputs = body.createVariableSettings()
    val atomsToIterate = variable.atoms.disc.map(a => ExhaustiveSearch.IndexedAtom(a,indexOfVar)).toIndexedSeq
    val iterator = new AllSettings(atomsToIterate)
    val bodyOutput = body.domain.createSetting()
    val bodyEval = body.evaluator()
  }

  def evaluator() = new SumProcessor with Evaluator{
    def eval(inputs: Array[Setting], output: Setting) = {
      output.cont(0) = 0.0
      this2body.copyForwardShallow(inputs,bodyInputs)
      //need to have sequence domain with variable length
      //this means that settings have to expand sometimes
      iterator.iterate(bodyInputs) {
        bodyEval.eval(bodyInputs,bodyOutput)
        output.cont(0) += bodyOutput.cont(0)
      }
    }
  }

  def differentiator(wrt: Seq[Var[Dom]]) = new SumProcessor with Differentiator{
    val eval = evaluator()

    def forwardProp(current: Array[Setting]) = {
      eval.eval(current,activation)
      this2body.copyForwardShallow(current,bodyInputs)
    }

    def term = sum
    def withRespectTo = wrt

    val bodyDiff = body.differentiator(wrt)
    val bodyGradient = body.createVariableSettings()

    def backProp(error: Setting, gradient: Array[Setting]) = {
      this2body.copyForwardShallow(gradient,bodyGradient)
      iterator.iterate(bodyInputs) {
        bodyDiff.addGradientAndValue(bodyInputs,error,bodyGradient,bodyOutput)
      }
      this2body.copyBackwardShallow(gradient,bodyGradient)
    }
  }
}

