package ml.wolfe.term

/**
 * @author riedel
 */
class Sum(val arguments: IndexedSeq[DoubleTerm]) extends ComposedDoubleTerm {

  sum =>

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
      full2Arg(currentIndex).copyForward(current, argInputs(currentIndex))
      argDiffs(currentIndex).forwardProp(argInputs(currentIndex))
      //take value of selected argument
      activation.cont(0) = argActivations(currentIndex).cont(0)
    }

    def backProp(error: Setting, gradient: Array[Setting]) = {
      localBackProp(argActivations, error, argErrors)
      if (arguments(currentIndex).vars.exists(withRespectTo.contains)) {
        full2Arg(currentIndex).copyForward(gradient, argGradients(currentIndex))
        argDiffs(currentIndex).backProp(argErrors(currentIndex), argGradients(currentIndex))
        full2Arg(currentIndex).copyBackward(gradient, argGradients(currentIndex))
      }
      //choose next index
    }
  }


}


trait DynamicTerm[D <: DoubleDom,T] extends ProxyTerm[D] {
  def generator:Generator[T]
  override def evaluator() = new Evaluator {
    val eval = self.evaluator()
    def eval(inputs: Array[Setting], output: Setting) = {
      generator.generateNext()
      eval.eval(inputs, output)
    }
  }
  override def differentiator(wrt: Seq[Var[Dom]]) = new Differentiator {
    val diff = self.differentiator(wrt)
    def forwardProp(current: Array[Setting]) = {
      generator.generateNext()
      diff.forwardProp(current)
    }
    def term = diff.term
    def withRespectTo = diff.withRespectTo
    def backProp(error: Setting, gradient: Array[Setting]) = {
      diff.backProp(error, gradient)
    }
  }
}

