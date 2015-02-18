package ml.wolfe.term

/**
 * @author riedel
 */
trait Argmaxer {
  def argmax(observed: Array[Setting], msgs: Array[Msgs], result: Array[Setting])
}

trait ArgmaxerFactory {
  def argmaxer(term: DoubleTerm, wrt: Seq[Var[Dom]]): Argmaxer
}

object Argmaxer {
  val exhaustive = new ArgmaxerFactory {
    def argmaxer(term: DoubleTerm, wrt: Seq[Var[Dom]]) = {
      require(wrt.forall(_.domain.isDiscrete), "exhaustive argmaxer needs discrete variables")
      new ExhaustiveSearchArgmaxer(term, wrt)
    }
  }
  def ascent(iterations: Int, learningRate: Double = 0.1, delta: Double = 0.0)(implicit initParams: Array[Setting] = Array()) = new ArgmaxerFactory {
    def argmaxer(term: DoubleTerm, wrt: Seq[Var[Dom]]) = {
      require(wrt.forall(_.domain.isContinuous), "ascent based argmaxer needs continuous variables")
      new AscentBasedArgmaxer(term, wrt, iterations, learningRate, initParams)
    }
  }

  def adaGrad(iterations: Int, learningRate: Double = 0.1, delta: Double = 0.0)(implicit initParams: Array[Setting] = Array()) = new ArgmaxerFactory {
    def argmaxer(term: DoubleTerm, wrt: Seq[Var[Dom]]) = {
      require(wrt.forall(_.domain.isContinuous), "adagrad based argmaxer needs continuous variables")
      new AdaGradArgmaxer(term, wrt, iterations, learningRate, delta, initParams)
    }
  }

}

trait MaxMarginalizer {
  def maxMarginals(observed:Array[Setting], in:Array[Msgs], out:Array[Msgs])
}


