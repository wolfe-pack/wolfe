package ml.wolfe.term

import scala.util.Random


class Sample[D <: Dom](val obj: DoubleTerm, val wrt: Var[D])(implicit random:Random) extends Term[D] {
  val domain = wrt.domain

  val vars = obj.vars.filter(_ != wrt)

  def isStatic = false

  override def evaluatorImpl(in: Settings) = new AbstractEvaluator(in) {
    val sampler = obj.samplerImpl(Seq(wrt))(in, null)

    def eval()(implicit execution: Execution) = {
      sampler.sample()
    }

    val output = sampler.result(0)
  }

  def by(factory: SamplerFactory) = {
    val newObj = new ProxyTerm[TypedDom[Double]] {
      def self = obj

      override def samplerImpl(wrt: Seq[Var[Dom]])(observed: Settings, msgs: Msgs)(implicit random:Random) = {
        factory.sampler(obj, wrt)(observed, msgs)
      }

      def copy(args: IndexedSeq[ArgumentType]) = ???
    }
    new Sample(newObj, wrt)
  }


}


