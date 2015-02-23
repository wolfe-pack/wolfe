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

  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.vect(0) = inputs(0).vect(0).mapValues(fun, output.vect(0))
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

