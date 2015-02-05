package ml.wolfe.term

/**
 * @author riedel
 */
trait Composed[D <: Dom] extends Term[D] {

  self =>

  lazy val vars = arguments.flatMap(_.vars).toSeq.distinct

  def arguments: IndexedSeq[Term[Dom]]

  def composer(): Evaluator

  def evaluator() = new Evaluator with Composer {
    val comp = composer()

    def eval(inputs: Array[Setting], output: Setting) = {
      for (i <- 0 until arguments.length) {
        full2Arg(i).copyForward(inputs, argInputs(i))
        argEvals(i).eval(argInputs(i), argOutputs(i))
      }
      comp.eval(argOutputs, output)
    }
  }

  def atoms = arguments.map(_.atoms).foldLeft(Atoms())(_ ++ _).distinct

  trait Composer {
    val argOutputs = arguments.map(_.domain.createSetting()).toArray
    val argInputs  = arguments.map(_.vars.map(_.domain.createSetting()).toArray)
    val full2Arg   = arguments.map(a => VariableMapping(vars, a.vars)).toArray
    val argEvals   = arguments.map(_.evaluator()).toArray
  }

  trait ComposedDifferentiator extends Differentiator with Composer {

    val term           = self
    val argErrors      = arguments.map(_.domain.createZeroSetting()).toArray
    val argGradients   = arguments.map(_.vars.map(_.domain.createZeroSetting()).toArray).toArray
    val argDiffs       = arguments.map(createDifferentiator).toArray
    val argActivations = argDiffs.map(_.activation)
    val comp           = composer()

    def createDifferentiator(term: Term[Dom]) =
      if (term.vars.exists(withRespectTo.contains)) term.differentiator(withRespectTo)
      else
        new EmptyDifferentiator(term, withRespectTo)


    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]): Unit

    //updates the activation of this term and all sub terms
    def forwardProp(current: Array[Setting]) = {
      for (i <- 0 until arguments.length) {
        full2Arg(i).copyForward(current, argInputs(i))
        argDiffs(i).forwardProp(argInputs(i))
      }
      comp.eval(argActivations, activation)

    }

    def backProp(error: Setting, gradient: Array[Setting]) = {
      localBackProp(argActivations, error, argErrors)
      for (i <- 0 until arguments.size) {
        if (arguments(i).vars.exists(withRespectTo.contains)) {
          full2Arg(i).copyForward(gradient, argGradients(i))
          argDiffs(i).backProp(argErrors(i), argGradients(i))
          full2Arg(i).copyBackward(gradient, argGradients(i))
        }
      }
    }
  }

}
