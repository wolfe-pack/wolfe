package ml.wolfe.term

/**
 * @author riedel
 */
trait Composed[+D <: Dom] extends Term[D] with NAry {

  self =>

  def composer(): EvaluatorOld

  def size = arguments.length

  lazy val vars = arguments.flatMap(_.vars).toSeq.distinct

  lazy val isStatic = arguments forall (_.isStatic)

  def evaluatorOld() = new EvaluatorOld with ComposerOld {
    val comp = composer()

    def eval(inputs: Array[Setting], output: Setting) = {
      for (i <- 0 until arguments.length) {
        full2Arg(i).copyForwardShallow(inputs, argInputs(i))
        argEvals(i).eval(argInputs(i), argOutputs(i))
      }
      comp.eval(argOutputs, output)
    }
  }

  abstract class Composer(in:Settings) extends AbstractEvaluator(in) {
    val output = self.domain.createSetting()
    def abort(index:Int) = false
  }

  def composer2(args: Settings): Composer = ???

  trait Cached {
    private var calls = 0
    def useCached = isStatic && calls > 0

    def cache[T](body: =>Unit): Unit = {
      if (!useCached) {
        body
      } else {
        println("Using cached value...")
      }
      calls += 1
    }
  }

  class ComposedEvaluator(in:Settings) extends Evaluator with Cached {
    val argEvals = arguments.map(a => a.evaluatorImpl(in.linkedSettings(vars, a.vars))).toArray
    val argOutputs = Settings.fromSeq(argEvals.map(_.output))
    val comp = composer2(argOutputs)
    val input = in
    val output = comp.output

    def eval()(implicit execution:Execution) = {
      cache {
        var i = 0
        while (i < size && !comp.abort(i)) {
          argEvals(i).eval()
          i += 1
        }
        comp.eval()
      }
    }
  }

  override def evaluatorImpl(in: Settings) = new ComposedEvaluator(in)

  abstract class ComposedDifferentiator(val wrt:Seq[Var[Dom]],
                                         val input: Settings,
                                         val error: Setting,
                                         val gradientAccumulator: Settings) extends Differentiator {


    def createArgError(a:ArgumentType) = {
      val result = a.domain.createSetting()
      result.setAdaptiveVectors(true)
      result.recordChangedOffsets = true
      result
    }

    val argDiffs = arguments.map(a => a.differentiatorImpl(wrt)(
      input.linkedSettings(vars,a.vars),createArgError(a),gradientAccumulator.linkedSettings(vars,a.vars))).toArray
    val argOutputs = Settings.fromSeq(argDiffs.map(_.output))
    val argErrors = Settings.fromSeq(argDiffs.map(_.error))
    val comp = composer2(argOutputs)
    val output = comp.output
    val mappings = arguments.map(a => VariableMapping(a.vars,vars))

    /**
     * update argErrors based on incoming error and current argOutputs (activations)
     */
    def localBackProp()(implicit execution:Execution)

    def abortForward(index:Int) = false
    def abortBackward(index:Int) = false

    def forward()(implicit execution:Execution) = {
      var i = 0
      while (i < size && !abortForward(i)) {
        argDiffs(i).forward()
        i += 1
      }
      comp.eval()
    }

    def backward()(implicit execution:Execution) = {
      localBackProp()
      var i = 0
      while (i < size && !abortBackward(i)) {
        argDiffs(i).backward()
        i += 1
      }
    }

  }

  def atomsIterator = arguments.iterator.flatMap(_.atomsIterator)

  trait ComposerOld {
    val argOutputs = arguments.map(_.domain.createSetting()).toArray
    val argInputs = arguments.map(_.vars.map(_.domain.createSetting()).toArray)
    //    val argInputs  = arguments.map(a => Array.ofDim[Setting](a.vars.length)).toArray
    val full2Arg = arguments.map(a => VariableMapping(vars, a.vars)).toArray
    val argEvals = arguments.map(_.evaluatorOld()).toArray
  }

  trait ComposedDifferentiatorOld extends DifferentiatorOld with ComposerOld {

    val term = self
    val argErrors = arguments.map(_.domain.createZeroSetting()).toArray
    //    val argGradients   = arguments.map(_.vars.map(_.domain.createSetting()).toArray).toArray
    val argGradients = arguments.map(a => Array.ofDim[Setting](a.vars.length)).toArray
    val argDiffs = arguments.map(createDifferentiator).toArray
    val argActivations = argDiffs.map(_.activation)
    val comp = composer()

    argErrors.foreach(_.setAdaptiveVectors(true))

    def createDifferentiator(term: Term[Dom]) =
      if (term.vars.exists(withRespectTo.contains)) term.differentiatorOld(withRespectTo)
      else
        new EmptyDifferentiatorOld(term, withRespectTo)


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

  def copy(args: IndexedSeq[ArgumentType]): Term[Dom]

}

trait Unary extends NAry {
  def argument:ArgumentType
  def copy(arg:ArgumentType):Term[Dom]

  def arguments = IndexedSeq(argument)

  def copy(args: IndexedSeq[ArgumentType]) = copy(args(0))
}
