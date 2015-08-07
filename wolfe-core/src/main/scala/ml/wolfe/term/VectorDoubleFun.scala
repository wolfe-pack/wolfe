package ml.wolfe.term

import cc.factorie.la.DenseTensor1
import ml.wolfe.util.Math._

/**
 * @author riedel
 */
class VectorDoubleFun[T <: VectorTerm](val arg: T, fun: Double => Double, deriv: Double => Double) extends Composed[GenericVectorDom] {
  self =>

  type ArgumentType = T

  import ml.wolfe.util.PimpMyFactorie._

  val arguments = IndexedSeq(arg)

  val domain: arg.domain.type = arg.domain

  def copy(args: IndexedSeq[ArgumentType]) = new VectorDoubleFun[T](args(0), fun, deriv)


  override def composer(args: Settings) = new Composer(args) {
    def eval()(implicit execution: Execution) = {
      input(0).vect(0).mapValues(fun, output.vect(0))
      //output.vect(0) = input(0).vect(0).mapValues(fun, output.vect(0))
    }
  }


  override def differentiatorImpl(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) =
    new ComposedDifferentiator(wrt,in,err,gradientAcc) {

      def localBackProp()(implicit execution: Execution) = {
        argOutputs(0).vect(0).mapValues(deriv, argErrors(0).vect(0))
        argOutputs(0).vect(0) :* error.vect(0)
      }
    }
}


class VectorSigmoid[T <: VectorTerm](override val arg: T)
  extends VectorDoubleFun(arg, sigmoid, sigmoidDeriv)

class VectorTanh[T <: VectorTerm](override val arg: T)
  extends VectorDoubleFun(arg, tanh, tanhDeriv)

class L1Norm[T <: VectorTerm](val arg:T) extends ComposedDoubleTerm {
  type ArgumentType = VectorTerm

  def copy(args: IndexedSeq[ArgumentType]) = new L1Norm(args(0))

  override def composer(in: Settings) = new Composer(in) {
    def eval()(implicit execution: Execution) = {
      output.cont(0) = input(0).vect(0).oneNorm
    }
  }

  val arguments = IndexedSeq(arg)


  override def differentiatorImpl(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) =
    new ComposedDifferentiator(wrt,in,err,gradientAcc) {

      def localBackProp()(implicit execution: Execution) = {
        val scale = error.cont(0)
        for (i <- 0 until argOutputs(0).vect(0).dim) {
          val current = argOutputs(0).vect(0)(i)
          argErrors(0).vect(0)(i) = if (current > 0.0) scale else if (current < 0) -scale else 0.0
        }
      }
    }

}
