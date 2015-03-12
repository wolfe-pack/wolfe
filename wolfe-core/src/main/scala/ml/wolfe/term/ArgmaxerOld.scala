package ml.wolfe.term

/**
 * @author riedel
 */
trait ArgmaxerOld {
  def argmax(observed: Array[Setting], msgs: Array[Msg], result: Array[Setting])
}

trait ArgmaxerImpl {
  val observed: Settings
  val msgs: Msgs
  val result: Settings

  def argmax()(implicit execution: Execution)
}

trait ArgmaxerFactoryOld {
  def argmaxer(term: DoubleTerm, wrt: Seq[Var[Dom]]): ArgmaxerOld
}

trait ArgmaxerFactory {
  def argmaxer(term: DoubleTerm, wrt: Seq[Var[Dom]])(obs: Settings, msgs: Msgs): ArgmaxerImpl
}


object ArgmaxerImpl {
  val exhaustive = new ArgmaxerFactoryOld {
    def argmaxer(term: DoubleTerm, wrt: Seq[Var[Dom]]) = {
      require(wrt.forall(_.domain.isDiscrete), "exhaustive argmaxer needs discrete variables")
      new ExhaustiveSearchArgmaxer(term, wrt)
    }
  }

  def ascent(iterations: Int, learningRate: Double = 0.1, delta: Double = 0.0,
             hook: (IndexedSeq[Any], Int) => String = null, delays: Map[Atom[Dom], Int] = Map.empty)(implicit initParams: Array[Setting] = Array()) = new ArgmaxerFactoryOld {
    def argmaxer(term: DoubleTerm, wrt: Seq[Var[Dom]]) = {
      require(wrt.forall(_.domain.isContinuous), "ascent based argmaxer needs continuous variables")
      new AscentBasedArgmaxer(term, wrt, iterations, learningRate, initParams)
    }
  }

  def adaGrad(iterations: Int, learningRate: Double = 0.1, delta: Double = 0.0,
              hook: (IndexedSeq[Any], Int) => String = null,
              delays: Map[Atom[Dom], Int] = Map.empty)(implicit initParams: Array[Setting] = Array()) = new ArgmaxerFactoryOld {
    def argmaxer(term: DoubleTerm, wrt: Seq[Var[Dom]]) = {
      require(wrt.forall(_.domain.isContinuous), "adagrad based argmaxer needs continuous variables")
      new AdaGradArgmaxer(term, wrt, iterations, learningRate, delta, initParams, hook, delays = delays)
    }
  }

  def adaGrad2(implicit params: AdaGradParameters) = new ArgmaxerFactory {
    def argmaxer(term: DoubleTerm, wrt: Seq[Var[Dom]])(obs: Settings, msgs: Msgs) =
      new AdaGradArgmaxer2(term, wrt, obs, msgs)
  }

  def maxProduct(implicit params: MaxProductParameters) = new ArgmaxerFactory {
    def argmaxer(term: DoubleTerm, wrt: Seq[Var[Dom]])(obs: Settings, msgs: Msgs) =
      new MaxProductBP(term, wrt, obs, msgs)(params)
  }


}


trait MaxMarginalizer2 {

  val input: Settings
  val inputMsgs: Msgs
  val outputMsgs: Msgs

  def maxMarginals()(implicit execution: Execution)
}


