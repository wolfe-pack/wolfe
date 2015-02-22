package ml.wolfe.term

/**
 * @author riedel
 */
class Sum(val arguments: IndexedSeq[DoubleTerm]) extends ComposedDoubleTerm {

  sum =>

  type ArgumentType = DoubleTerm

  def copy(args: IndexedSeq[ArgumentType]) = new Sum(args)

  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.cont(0) = 0.0
      for (i <- 0 until inputs.length)
        output.cont(0) += inputs(i).cont(0)
    }
  }

  def differentiator(wrt: Seq[Var[Dom]]) = new ComposedDifferentiator {
    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]) = {
      for (i <- 0 until argOutputs.length)
        gradient(i).cont(0) = outError.cont(0)
    }

    def withRespectTo = wrt
  }

  def sequentialGradient() = new ProxyTerm[DoubleDom] {
    def self = sum
    override def differentiator(wrt: Seq[Var[Dom]]) = new StochasticDifferentiator {
      private var _index = 0
      def selectIndex() = {
        if (_index == arguments.length) {
          _index = 0
        }
        val tmp = _index
        _index += 1
        tmp
      }
      def withRespectTo = wrt
    }
  }

  trait StochasticDifferentiator extends Differentiator with Composer {

    val term           = sum
    val argErrors      = arguments.map(_.domain.createZeroSetting()).toArray
    val argGradients   = arguments.map(_.vars.map(_.domain.createZeroSetting()).toArray).toArray
    val argDiffs       = arguments.map(createDifferentiator).toArray
    val argActivations = argDiffs.map(_.activation)
    val comp           = composer()

    def selectIndex(): Int

    var currentIndex = -1

    def createDifferentiator(term: Term[Dom]) =
      if (term.vars.exists(withRespectTo.contains)) term.differentiator(withRespectTo)
      else
        new EmptyDifferentiator(term, withRespectTo)


    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]) = {
      gradient(currentIndex).cont(0) = outError.cont(0)
    }

    //updates the activation of this term and all sub terms
    def forwardProp(current: Array[Setting]) = {
      currentIndex = selectIndex()
      full2Arg(currentIndex).copyForwardDeep(current, argInputs(currentIndex))
      argDiffs(currentIndex).forwardProp(argInputs(currentIndex))
      //take value of selected argument
      activation.cont(0) = argActivations(currentIndex).cont(0)
    }

    def backProp(error: Setting, gradient: Array[Setting]) = {
      localBackProp(argActivations, error, argErrors)
      if (arguments(currentIndex).vars.exists(withRespectTo.contains)) {
        full2Arg(currentIndex).copyForwardDeep(gradient, argGradients(currentIndex))
        argDiffs(currentIndex).backProp(argErrors(currentIndex), argGradients(currentIndex))
        full2Arg(currentIndex).copyBackwardDeep(gradient, argGradients(currentIndex))
      }
      //choose next index
    }
  }


}

/**
 * @author riedel
 */
class VectorSum(val arguments: IndexedSeq[VectorTerm]) extends Composed[VectorDom] {

  sum =>

  type ArgumentType = VectorTerm

  def copy(args: IndexedSeq[ArgumentType]) = new VectorSum(args)

  val domain = arguments.head.domain

  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      output := 0.0
      for (i <- 0 until inputs.length)
        output.vect(0) += inputs(i).vect(0)
    }
  }

  def differentiator(wrt: Seq[Var[Dom]]) = new ComposedDifferentiator {
    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]) = {
      for (i <- 0 until argOutputs.length)
        gradient(i).setVect(0,outError.vect(0))
    }

    def withRespectTo = wrt
  }


}



