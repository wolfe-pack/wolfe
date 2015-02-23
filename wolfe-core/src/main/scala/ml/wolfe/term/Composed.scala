package ml.wolfe.term

/**
 * @author riedel
 */
trait Composed[+D <: Dom] extends Term[D] with NAry {

  self =>

  def composer(): Evaluator

  lazy val vars = arguments.flatMap(_.vars).toSeq.distinct

  def evaluator() = new Evaluator with Composer {
    val comp = composer()

    def eval(inputs: Array[Setting], output: Setting) = {
      for (i <- 0 until arguments.length) {
        full2Arg(i).copyForwardShallow(inputs, argInputs(i))
        argEvals(i).eval(argInputs(i), argOutputs(i))
      }
      comp.eval(argOutputs, output)
    }
  }

  def atomsIterator = arguments.iterator.flatMap(_.atomsIterator)

  trait Composer {
    val argOutputs = arguments.map(_.domain.createSetting()).toArray
    val argInputs  = arguments.map(_.vars.map(_.domain.createSetting()).toArray)
//    val argInputs  = arguments.map(a => Array.ofDim[Setting](a.vars.length)).toArray
    val full2Arg   = arguments.map(a => VariableMapping(vars, a.vars)).toArray
    val argEvals   = arguments.map(_.evaluator()).toArray
  }

  trait ComposedDifferentiator extends Differentiator with Composer {

    val term           = self
    val argErrors      = arguments.map(_.domain.createZeroSetting()).toArray
//    val argGradients   = arguments.map(_.vars.map(_.domain.createSetting()).toArray).toArray
    val argGradients   = arguments.map(a => Array.ofDim[Setting](a.vars.length)).toArray
    val argDiffs       = arguments.map(createDifferentiator).toArray
    val argActivations = argDiffs.map(_.activation)
    val comp           = composer()

    argErrors.foreach(_.setAdaptiveVectors(true))

    def createDifferentiator(term: Term[Dom]) =
      if (term.vars.exists(withRespectTo.contains)) term.differentiator(withRespectTo)
      else
        new EmptyDifferentiator(term, withRespectTo)


    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]): Unit

    //updates the activation of this term and all sub terms
    def forwardProp(current: Array[Setting]) = {
      for (i <- 0 until arguments.length) {
        full2Arg(i).copyForwardShallow(current, argInputs(i))
        argDiffs(i).forwardProp(argInputs(i))
      }
      comp.eval(argActivations, activation)

    }

    def backProp(error: Setting, gradient: Array[Setting]) = {
      localBackProp(argActivations, error, argErrors)
      for (i <- 0 until arguments.size) {
        if (arguments(i).vars.exists(withRespectTo.contains)) {
          full2Arg(i).copyForwardShallow(gradient, argGradients(i))
          argDiffs(i).backProp(argErrors(i), argGradients(i))
          full2Arg(i).copyBackwardShallow(gradient, argGradients(i))
        }
      }
    }
  }

}

trait NAry {
  type ArgumentType <: Term[Dom]
  def arguments: IndexedSeq[ArgumentType]
  def copy(args:IndexedSeq[ArgumentType]):Term[Dom]

}
