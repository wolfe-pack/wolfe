package ml.wolfe.term

/**
 * @author riedel
 */
trait ArgmaxerOld {
  def argmax(observed: Array[Setting], msgs: Array[Msg], result: Array[Setting])
}

trait Argmaxer {
  val observed: Settings
  val msgs: Msgs
  val result: Settings

  def argmax()(implicit execution: Execution)
}

trait ArgmaxerFactoryOld {
  def argmaxer(term: DoubleTerm, wrt: Seq[Var[Dom]]): ArgmaxerOld
}

trait ArgmaxerFactory {
  def argmaxer(term: DoubleTerm, wrt: Seq[Var[Dom]])(obs: Settings, msgs: Msgs): Argmaxer
}


object Argmaxer {

  def adaGrad2(implicit params: AdaGradParameters) = new ArgmaxerFactory {
    def argmaxer(term: DoubleTerm, wrt: Seq[Var[Dom]])(obs: Settings, msgs: Msgs) =
      new AdaGradArgmaxer(term, wrt, obs, msgs)
  }

  def maxProduct(implicit params: MaxProductParameters) = new ArgmaxerFactory {
    def argmaxer(term: DoubleTerm, wrt: Seq[Var[Dom]])(obs: Settings, msgs: Msgs) =
      new MaxProductBP(term, wrt, obs, msgs)(params)
  }


}


trait MaxMarginalizer {

  val input: Settings
  val inputMsgs: Msgs
  val outputMsgs: Msgs

  def maxMarginals()(implicit execution: Execution)
}


