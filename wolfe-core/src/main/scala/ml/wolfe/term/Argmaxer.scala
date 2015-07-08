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


trait MarginalizerFactory {
  def marginalizer(term: DoubleTerm, wrt: Seq[AnyVar], observed: Seq[AnyVar])
                  (input: Settings, inputMsgs: Msgs, reverseMsgsAlso: Boolean): Marginalizer
}


object Argmaxer {

  def adaGrad(implicit params: AdaGradParameters) = new ArgmaxerFactory {
    def argmaxer(term: DoubleTerm, wrt: Seq[Var[Dom]])(obs: Settings, msgs: Msgs) =
      new AdaGradArgmaxer(term, wrt, obs, msgs)
  }

  def maxProduct(implicit params: BPParameters) = new ArgmaxerFactory {
    def argmaxer(term: DoubleTerm, wrt: Seq[Var[Dom]])(obs: Settings, msgs: Msgs) =
      new MaxProductBP(term, wrt, obs, msgs)(params)
  }

  def bruteForce = new ArgmaxerFactory {
    def argmaxer(term: DoubleTerm, wrt: Seq[Var[Dom]])(obs: Settings, msgs: Msgs) =
      new ExhaustiveSearchArgmaxer(term, wrt)(obs, msgs)
  }

}

object Marginalizer {
  def sumProduct(implicit params:BPParameters = BPParameters(2)) = new MarginalizerFactory {
    def marginalizer(term: DoubleTerm, wrt: Seq[AnyVar],
                     observed: Seq[AnyVar])(input: Settings, inputMsgs: Msgs, reverseMsgsAlso: Boolean) = {
      require(reverseMsgsAlso)
      new SumProductBP(term,wrt,input,inputMsgs)
    }
  }

  def bruteForce = new MarginalizerFactory {
    def marginalizer(term: DoubleTerm, wrt: Seq[AnyVar], observed: Seq[AnyVar])(
      input: Settings, inputMsgs: Msgs, reverseMsgsAlso: Boolean) = {
      new ExhaustiveSearchMarginalizer(term, wrt,observed,input,inputMsgs,reverseMsgsAlso)
    }
  }

}

trait MessageCalculator {
  val input: Settings
  val inputMsgs: Msgs
  val outputMsgs: Msgs

  def updateMessages()(implicit execution: Execution)


}

trait MaxMarginalizer extends MessageCalculator {

}

trait Marginalizer extends MessageCalculator {

}


