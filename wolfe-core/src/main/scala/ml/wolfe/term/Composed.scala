package ml.wolfe.term

/**
 * @author riedel
 */
trait Composed[+D <: Dom] extends Term[D] with NAry {

  self =>


  def size = arguments.length

  lazy val vars = arguments.flatMap(_.vars).toSeq.distinct

  lazy val isStatic = arguments forall (_.isStatic)

  abstract class Composer(in: Settings) extends AbstractEvaluator(in) {
    val output = self.domain.createSetting()

    def abort(index: Int) = false
  }

  def composer(args: Settings): Composer = ???

  class ComposedEvaluator(in: Settings) extends Evaluator {
    val argEvals = arguments.map(a => a.evaluatorImpl(in.linkedSettings(vars, a.vars))).toArray
    val argOutputs = Settings.fromSeq(argEvals.map(_.output))
    val comp = composer(argOutputs)
    val input = in
    val output = comp.output

    def eval()(implicit execution: Execution) = {
      var i = 0
      while (i < size && !comp.abort(i)) {
        argEvals(i).eval()
        i += 1
      }
      comp.eval()
    }

    override def tree:Seq[ml.wolfe.term.Evaluator] = {
      this +: argEvals.flatMap(_.tree).toSeq
    }

  }

  override def evaluatorImpl(in: Settings) = new ComposedEvaluator(in)

  abstract class ComposedDifferentiator(val withRespectTo: Seq[Var[Dom]],
                                        val input: Settings,
                                        val error: Setting,
                                        val gradientAccumulator: Settings) extends Differentiator {


    def createArgError(a: ArgumentType) = {
      val result = a.domain.createSetting()
      result.setAdaptiveVectors(true)
      result.informListeners = true
      result
    }

    def createArgDiff(a: ArgumentType): a.Differentiator =
      if (a.vars.isEmpty || withRespectTo.forall(!a.vars.contains(_)))
        new a.EmptyDifferentiator(input.linkedSettings(vars, a.vars), createArgError(a), gradientAccumulator.linkedSettings(vars, a.vars), withRespectTo)
      else
        a.differentiatorImpl(withRespectTo)(
          input.linkedSettings(vars, a.vars), createArgError(a), gradientAccumulator.linkedSettings(vars, a.vars))

    val argDiffs = arguments.map(createArgDiff(_)).toArray
    val argOutputs = Settings.fromSeq(argDiffs.map(_.output))
    val argErrors = Settings.fromSeq(argDiffs.map(_.error))
    val comp = composer(argOutputs)
    val output = comp.output
    val mappings = arguments.map(a => VariableMapping(a.vars, vars))

    /**
     * update argErrors based on incoming error and current argOutputs (activations)
     */
    def localBackProp()(implicit execution: Execution)

    def abortForward(index: Int) = false

    def abortBackward(index: Int) = false

    def forward()(implicit execution: Execution) = {
      var i = 0
      while (i < size && !abortForward(i)) {
        argDiffs(i).forward()
        i += 1
      }
      comp.eval()
    }

    def backward()(implicit execution: Execution) = {
      localBackProp()
      var i = 0
      while (i < size && !abortBackward(i)) {
        argDiffs(i).optimizedBackward()
        i += 1
      }
    }

  }

}

class ManualTerm[D <: Dom](val compose: (Settings, Setting) => Unit, val arguments: IndexedSeq[AnyTerm], val domain: D) extends Composed[D] {

  type ArgumentType = AnyTerm

  def copy(args: IndexedSeq[ArgumentType]) = new ManualTerm[D](compose,args,domain)

  override def composer(args: Settings) = new Composer(args) {
    def eval()(implicit execution: Execution) = {
      compose(input, output)
    }
  }
}

trait NAry extends Term[Dom] {
  type ArgumentType <: Term[Dom]

  def arguments: IndexedSeq[ArgumentType]

  def copy(args: IndexedSeq[ArgumentType]): Term[Dom]

  def copyIfDifferent(args: IndexedSeq[ArgumentType]) : Term[Dom] = {
    if (args == arguments) this else copy(args)
  }

}

trait Unary extends NAry {
  def argument: ArgumentType

  def copy(arg: ArgumentType): Term[Dom]

  def arguments = IndexedSeq(argument)

  def copy(args: IndexedSeq[ArgumentType]) = copy(args(0))
}
