package ml.wolfe.term

import ml.wolfe.util.Math._

/**
 * @author riedel
 */
class DoubleFun[T <: DoubleTerm](val arg: T, fun: Double => Double, deriv: Double => Double) extends ComposedDoubleTerm {
  self =>

  type ArgumentType = T

  val arguments = IndexedSeq(arg)


  def copy(args: IndexedSeq[ArgumentType]) = new DoubleFun[T](args(0), fun, deriv)

  def composer() = new EvaluatorOld {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.cont(0) = fun(inputs(0).cont(0))
    }
  }

  def differentiatorOld(wrt: Seq[Var[Dom]]) = new ComposedDifferentiatorOld {
    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]) = {
      gradient(0).cont(0) = deriv(argOutputs(0).cont(0)) * outError.cont(0)
    }

    def withRespectTo = wrt
  }

  override def composer2(args: Settings) = new Composer(args) {
    def eval()(implicit execution: Execution) = {
      output.cont(0) = fun(input(0).cont(0))
    }
  }

  override def differentiatorImpl(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) =
    new ComposedDifferentiator(wrt,in,err,gradientAcc) {
      def localBackProp()(implicit execution: Execution) = {
        argErrors(0).cont(0) = deriv(argOutputs(0).cont(0)) * error.cont(0)
      }
    }
}

/**
 * @author riedel
 */
class DoubleBinaryFun[T <: DoubleTerm](val arg1: T, arg2:T, fun: (Double,Double) => Double,
                                            deriv: (Double,Double) => (Double,Double)) extends ComposedDoubleTerm {
  self =>

  type ArgumentType = T

  val arguments = IndexedSeq(arg1,arg2)
  
  def copy(args: IndexedSeq[ArgumentType]) = new DoubleBinaryFun[T](args(0),args(1),fun, deriv)

  def composer() = new EvaluatorOld {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.cont(0) = fun(inputs(0).cont(0),inputs(1).cont(0))
    }
  }

  def differentiatorOld(wrt: Seq[Var[Dom]]) = new ComposedDifferentiatorOld {
    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]) = {
      val (d1,d2) = deriv(argOutputs(0).cont(0),argOutputs(1).cont(0))
      gradient(0).cont(0) = d1 * outError.cont(0)
      gradient(1).cont(0) = d2 * outError.cont(0)
    }

    def withRespectTo = wrt
  }

  override def composer2(args: Settings) = new Composer(args) {
    def eval()(implicit execution: Execution) = {
      output.cont(0) = fun(input(0).cont(0), input(1).cont(0))
    }
  }

  override def differentiatorImpl(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) =
    new ComposedDifferentiator(wrt,in,err,gradientAcc) {
      def localBackProp()(implicit execution: Execution) = {
        val (d1,d2) = deriv(argOutputs(0).cont(0),argOutputs(1).cont(0))
        argErrors(0).cont(0) = d1 * error.cont(0)
        argErrors(1).cont(0) = d2 * error.cont(0)
      }
    }
}


class Sigmoid[T <: DoubleTerm](override val arg: T) extends DoubleFun(arg, sigmoid, sigmoidDeriv)

class Sqrt[T <: DoubleTerm](override val arg: T) extends DoubleFun(arg, math.sqrt, x => 0.5 / math.sqrt(x))

class Rectifier[T <:DoubleTerm](arg:T) extends DoubleFun(arg,x => if (x > 0.0) x else 0.0, x => if (x > 0.0) 1.0 else 0.0)

class Log[T <: DoubleTerm](override val arg: T) extends DoubleFun(arg, math.log, logDeriv)

class Tanh[T <: DoubleTerm](override val arg: T) extends DoubleFun(arg, tanh, tanhDeriv)

class Min2[T<:DoubleTerm](arg1:T,arg2:T) extends DoubleBinaryFun(arg1,arg2,
  (x1:Double,x2:Double) => math.min(x1,x2),
  (x1:Double,x2:Double) => if (x1 < x2) (1.0,0.0) else (0.0,1.0))

class Max2[T<:DoubleTerm](arg1:T,arg2:T) extends DoubleBinaryFun(arg1,arg2,
  (x1:Double,x2:Double) => math.max(x1,x2),
  (x1:Double,x2:Double) => if (x1 > x2) (1.0,0.0) else (0.0,1.0))

