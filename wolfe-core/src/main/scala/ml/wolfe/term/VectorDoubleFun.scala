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


  override def composer2(args: Settings) = new Composer2(args) {
    def eval() = {
      output.vect(0) = input(0).vect(0).mapValues(fun, output.vect(0))
    }
  }

  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.vect(0) = inputs(0).vect(0).mapValues(fun, output.vect(0))
    }
  }


  override def differentiator2(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) =
    new ComposedDifferentiator2(wrt,in,err,gradientAcc) {

      def localBackProp() = {
        argErrors(0).vect(0) = argOutputs(0).vect(0).mapValues(deriv, argErrors(0).vect(0)) :* error.vect(0).asInstanceOf[DenseTensor1]

      }
    }

  def differentiator(wrt: Seq[Var[Dom]]) = new ComposedDifferentiator {
    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]) = {
      gradient(0).vect(0) = argOutputs(0).vect(0).mapValues(deriv, gradient(0).vect(0)) :* outError.vect(0).asInstanceOf[DenseTensor1]
    }

    def withRespectTo = wrt
  }
}


class VectorSigmoid[T <: VectorTerm](override val arg: T)
  extends VectorDoubleFun(arg, sigmoid, sigmoidDeriv)

class VectorTanh[T <: VectorTerm](override val arg: T)
  extends VectorDoubleFun(arg, tanh, tanhDeriv)

class L1Norm[T <: VectorTerm](val arg:T) extends ComposedDoubleTerm {
  type ArgumentType = VectorTerm

  def copy(args: IndexedSeq[ArgumentType]) = new L1Norm(args(0))

  def composer() = new Evaluator {

    def eval(inputs: Array[Setting], output: Setting) = {
      output.cont(0) = inputs(0).vect(0).oneNorm
    }
  }


  override def composer2(in: Settings) = new Composer2(in) {
    def eval() = {
      output.cont(0) = input(0).vect(0).oneNorm
    }
  }

  val arguments = IndexedSeq(arg)


  override def differentiator2(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) =
    new ComposedDifferentiator2(wrt,in,err,gradientAcc) {

      def localBackProp() = {
        val scale = error.cont(0)
        for (i <- 0 until argOutputs(0).vect(0).dim1) {
          val current = argOutputs(0).vect(0)(i)
          argErrors(0).vect(0)(i) = if (current > 0.0) scale else if (current < 0) -scale else 0.0
        }
      }
    }

  def differentiator(wrt: Seq[Var[Dom]]) = new ComposedDifferentiator {
    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]) = {
      val scale = outError.cont(0)
      for (i <- 0 until argOutputs(0).vect(0).dim1) {
        val current = argOutputs(0).vect(0)(i)
        gradient(0).vect(0)(i) = if (current > 0.0) scale else if (current < 0) -scale else 0.0
      }
    }

    def withRespectTo = wrt
  }
}
