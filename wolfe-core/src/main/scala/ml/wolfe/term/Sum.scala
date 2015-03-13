package ml.wolfe.term

import cc.factorie.la.DenseTensor1

/**
 * @author riedel
 */
case class Sum(arguments: IndexedSeq[DoubleTerm]) extends ComposedDoubleTerm {

  sum =>

  type ArgumentType = DoubleTerm

  def copy(args: IndexedSeq[ArgumentType]) = new Sum(args)

  def composer() = new EvaluatorOld {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.cont(0) = 0.0
      for (i <- 0 until inputs.length)
        output.cont(0) += inputs(i).cont(0)
    }
  }

  def differentiatorOld(wrt: Seq[Var[Dom]]) = new ComposedDifferentiatorOld {
    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]) = {
      for (i <- 0 until argOutputs.length)
        gradient(i).cont(0) = outError.cont(0)
    }

    def withRespectTo = wrt
  }

  def sequentialGradient() = new ProxyTerm[DoubleDom] {
    def self = sum

    def copy(args: IndexedSeq[ArgumentType]) = ???

    override def differentiatorOld(wrt: Seq[Var[Dom]]) = new StochasticDifferentiatorOld {
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

  trait StochasticDifferentiatorOld extends DifferentiatorOld with Composer {

    val term = sum
    val argErrors = arguments.map(_.domain.createZeroSetting()).toArray
    val argGradients = arguments.map(_.vars.map(_.domain.createZeroSetting()).toArray).toArray
    val argDiffs = arguments.map(createDifferentiator).toArray
    val argActivations = argDiffs.map(_.activation)
    val comp = composer()

    def selectIndex(): Int

    var currentIndex = -1

    def createDifferentiator(term: Term[Dom]) =
      if (term.vars.exists(withRespectTo.contains)) term.differentiatorOld(withRespectTo)
      else
        new EmptyDifferentiatorOld(term, withRespectTo)


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

  override def composer2(args: Settings) = new Composer2(args) {
    def eval()(implicit execution: Execution): Unit = {
      output.cont(0) = 0.0
      for (i <- 0 until size) output.cont(0) += input(i).cont(0)
    }
  }

  override def differentiatorImpl(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) = new Differentiator {


    val input = in
    val error = err
    val gradientAccumulator = gradientAcc

    val argDiffs = for (a <- arguments) yield
      a.differentiatorImpl(wrt)(input.linkedSettings(vars, a.vars), err, gradientAccumulator.linkedSettings(vars, a.vars))
    val argOutputs = Settings.fromSeq(argDiffs.map(_.output))
    val comp = composer2(argOutputs)
    val output = comp.output

    def forward()(implicit execution: Execution) = {
      argDiffs foreach (_.forward())
      comp.eval()
    }

    def backward()(implicit execution: Execution) = {
      argDiffs foreach (_.backward())
    }
  }
}

case class VarSeqSum[D <: TypedDom[Double], T <: Term[VarSeqDom[D]]](seq: T) extends ComposedDoubleTerm {

  self =>

  type ArgumentType = Term[Dom]

  val (length, elements) = seq match {
    case s: seq.domain.Constructor =>
      s.length -> s.elements
    case _ =>
      val l = new VarSeqLength[T](seq)
      val e = for (i <- 0 until seq.domain.maxLength) yield
        new VarSeqApply[D, T, TypedTerm[Int]](seq, seq.domain.lengthDom.Const(i))
      l -> e
  }

  val arguments = length +: elements

  def copy(args: IndexedSeq[ArgumentType]) = ???


  override def composer2(args: Settings) = new Composer2(args) {
    def eval()(implicit execution: Execution) = {
      val length = input(0).disc(0)
      output.cont(0) = 0.0
      for (i <- 0 until length) output.cont(0) += input(i+1).cont(0)
    }

    override def abort(index: Int) = {
      index > 0 && index > input(0).disc(0)
    }
  }


  override def differentiatorImpl(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) =
    new ComposedDifferentiator(wrt,in,err,gradientAcc) {

      def localBackProp()(implicit execution: Execution) = {
        val length = argOutputs(0).disc(0)
        for (i <- 0 until length) {
          argErrors(i+1) := error
        }
      }

      override def abortForward(index: Int) = {
        index > 0 && index > argOutputs(0).disc(0)
      }

      override def abortBackward(index: Int) = abortForward(index)
    }

  trait Composer {
    val argOutputs = arguments.map(_.domain.createSetting()).toArray
    val argInputs = arguments.map(_.vars.map(_.domain.createSetting()).toArray)
    //    val argInputs  = arguments.map(a => Array.ofDim[Setting](a.vars.length)).toArray
    val full2Arg = arguments.map(a => VariableMapping(vars, a.vars)).toArray
    val argEvals = arguments.map(_.evaluatorOld()).toArray
  }


  def composer() = ???

  def differentiatorOld(wrt: Seq[Var[Dom]]) = new ComposedDifferentiatorOld {
    def withRespectTo = wrt
  }

  //todo: avoid code duplication here
  trait ComposedDifferentiatorOld extends DifferentiatorOld with Composer {

    val term = self
    val argErrors = arguments.map(_.domain.createZeroSetting()).toArray
    //    val argGradients   = arguments.map(_.vars.map(_.domain.createSetting()).toArray).toArray
    val argGradients = arguments.map(a => Array.ofDim[Setting](a.vars.length)).toArray
    val argDiffs = arguments.map(createDifferentiator).toArray
    val argActivations = argDiffs.map(_.activation)

    argErrors.foreach(_.setAdaptiveVectors(true))

    def createDifferentiator(term: Term[Dom]) =
      if (term.vars.exists(withRespectTo.contains)) term.differentiatorOld(withRespectTo)
      else
        new EmptyDifferentiatorOld(term, withRespectTo)


    //updates the activation of this term and all sub terms
    def forwardProp(current: Array[Setting]) = {
      full2Arg(0).copyForwardShallow(current, argInputs(0))
      argDiffs(0).forwardProp(argInputs(0))
      val length = argActivations(0).disc(0)
      var result = 0.0

      for (i <- 1 until length + 1) {
        full2Arg(i).copyForwardShallow(current, argInputs(i))
        argDiffs(i).forwardProp(argInputs(i))
        result += argActivations(i).cont(0)
      }
      activation.cont(0) = result
    }

    def backProp(error: Setting, gradient: Array[Setting]) = {

      val length = argActivations(0).disc(0)
      for (i <- 1 until length + 1) {
        if (arguments(i).vars.exists(withRespectTo.contains)) {
          argErrors(i).cont(0) = error.cont(0)
          full2Arg(i).copyForwardShallow(gradient, argGradients(i))
          argDiffs(i).backProp(argErrors(i), argGradients(i))
          full2Arg(i).copyBackwardShallow(gradient, argGradients(i))
        }
      }
    }
  }

}

/**
 * @author riedel
 */
class VectorSum(val arguments: IndexedSeq[VectorTerm]) extends Composed[GenericVectorDom] {

  sum =>

  type ArgumentType = VectorTerm

  def copy(args: IndexedSeq[ArgumentType]) = new VectorSum(args)

  val domain = arguments.head.domain

  def composer() = new EvaluatorOld {
    def eval(inputs: Array[Setting], output: Setting) = {
      if (output.vect(0) == null) output.vect(0) = new DenseTensor1(domain.dim)
      else output := 0.0
      for (i <- 0 until inputs.length)
        output.vect(0) += inputs(i).vect(0)
    }
  }

  def differentiatorOld(wrt: Seq[Var[Dom]]) = new ComposedDifferentiatorOld {
    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]) = {
      for (i <- 0 until argOutputs.length)
        gradient(i).vect.update(0, outError.vect(0))
    }

    def withRespectTo = wrt
  }

}



