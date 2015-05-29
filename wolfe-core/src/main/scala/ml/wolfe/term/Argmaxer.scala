package ml.wolfe.term

import scala.util.Random


trait Argmaxer {
  val observed: Settings
  val msgs: Msgs
  val result: Settings

  def argmax()(implicit execution: Execution)
}

trait Sampler {
  val observed: Settings
  val msgs: Msgs
  val result: Settings

  def sample()(implicit execution: Execution)
}


trait ArgmaxerFactory {
  def argmaxer(term: DoubleTerm, wrt: Seq[Var[Dom]])(obs: Settings, msgs: Msgs): Argmaxer
}

trait SamplerFactory {
  def sampler(term: DoubleTerm, wrt: Seq[Var[Dom]])(obs: Settings, msgs: Msgs)(implicit random: Random): Sampler
}


object Argmaxer {

  def adaGrad(implicit params: AdaGradParameters) = new ArgmaxerFactory {
    def argmaxer(term: DoubleTerm, wrt: Seq[Var[Dom]])(obs: Settings, msgs: Msgs) =
      new AdaGradArgmaxer(term, wrt, obs, msgs)
  }

  def maxProduct(implicit params: MaxProductParameters) = new ArgmaxerFactory {
    def argmaxer(term: DoubleTerm, wrt: Seq[Var[Dom]])(obs: Settings, msgs: Msgs) =
      new MaxProductBP(term, wrt, obs, msgs)(params)
  }

  def bruteForce = new ArgmaxerFactory {
    def argmaxer(term: DoubleTerm, wrt: Seq[Var[Dom]])(obs: Settings, msgs: Msgs) =
      new ExhaustiveSearchArgmaxer(term, wrt)(obs, msgs)
  }


}

trait MessageCalculator {
  val input: Settings
  val inputMsgs: Msgs
  val outputMsgs: Msgs

}

trait MaxMarginalizer extends MessageCalculator {

  def maxMarginals()(implicit execution: Execution)
}

trait Marginalizer extends MessageCalculator {

  def marginals()(implicit execution: Execution)

}


