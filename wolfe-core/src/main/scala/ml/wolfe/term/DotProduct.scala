package ml.wolfe.term

import cc.factorie.la.{DenseTensor2, SparseIndexedTensor1}
import ml.wolfe.Vect
import scalaxy.loops._
import scala.language.postfixOps

/**
 * @author riedel
 */
class DotProduct[T1 <: VectorTerm, T2 <: VectorTerm](val arg1: T1, val arg2: T2) extends ComposedDoubleTerm {

  self =>

  type ArgumentType = VectorTerm

  val arguments = IndexedSeq(arg1, arg2)

  def copy(args: IndexedSeq[ArgumentType]) = new DotProduct(args(0), args(1))

  def sparseDot(s1: SparseIndexedTensor1, arg2: Vect) = {
    var result = 0.0
    for (i <- 0 until s1._unsafeActiveDomainSize optimized) {
      val index = s1._indices(i)
      val value = s1._values(i)
      result += value * arg2(index)
    }
    result
  }

  def ownDot(arg1: Vect, arg2: Vect) = {
    arg1 match {
      case s1: SparseIndexedTensor1 => sparseDot(s1, arg2)
      case _ =>
        arg2 match {
          case s2: SparseIndexedTensor1 => sparseDot(s2, arg1)
          case _ => arg1 dot arg2
        }
    }
  }

  override def composer(args: Settings) = new Composer(args) {
    def eval()(implicit execution: Execution) = {
//      output.cont(0) = ownDot(input(0).vect(0), input(1).vect(0))
      output.cont(0) = input(0).vect(0) dot input(1).vect(0)
    }
  }


  override def differentiatorImpl(wrt: Seq[AnyVar])(in: Settings, err: Setting, gradientAcc: Settings) =
    new ComposedDifferentiator(wrt, in, err, gradientAcc) {

      val diffArg1 = wrt.exists(arg1.vars.contains)
      val diffArg2 = wrt.exists(arg2.vars.contains)

      def localBackProp()(implicit execution: Execution) = {

        val scale = error.cont(0)

        if (diffArg1) argErrors(0).vect.set(0, argOutputs(1).vect(0), scale)
        if (diffArg2) argErrors(1).vect.set(0, argOutputs(0).vect(0), scale)

      }
    }

  override def toString = s"$arg1 dot $arg2"
}


class SparseL2[T1 <: VectorTerm, T2 <: VectorTerm](val arg: T1, val mask: T2 = null) extends ComposedDoubleTerm {

  self =>

  type ArgumentType = VectorTerm

  val arguments = if (mask == null) IndexedSeq(arg) else IndexedSeq(arg, mask)

  def copy(args: IndexedSeq[ArgumentType]) =
    if (mask == null) new SparseL2(args(0), null) else new SparseL2(args(0), args(1))

  override def composer(args: Settings) = new Composer(args) {
    def eval()(implicit execution: Execution) = {
      val w = input(0).vect(0)
      if (mask != null) {
        val f = input(1).vect(0)
        output.cont(0) = 0.0
        f.foreachActiveElement { case (i, v) =>
          output.cont(0) += w(i) * w(i) * v
        }
      } else {
        output.cont(0) = w dot w
      }
    }
  }


  override def differentiatorImpl(wrt: Seq[AnyVar])(in: Settings, err: Setting, gradientAcc: Settings) =
    new ComposedDifferentiator(wrt, in, err, gradientAcc) {

      def localBackProp()(implicit execution: Execution) = {

        val scale = error.cont(0)

        val w = argOutputs(0).vect(0)

        if (mask != null) {
          val f = argOutputs(1).vect(0)
          import ml.wolfe.util.PimpMyFactorie._
          //need to multiply w with f
          argErrors(0).vect.update(0, f)
          argErrors(0).vect(0) :* w
          argErrors(0).vect(0) *= 2.0 * scale
        } else {
          argErrors(0).vect.update(0, w)
          argErrors(0).vect(0) *= scale * 2.0
        }
        //todo: calculate gradient for mask!
      }
    }
}

//rockt: this should be generalized to vectors and matrices
class MatrixDotProduct[T1 <: Term[MatrixDom], T2 <: Term[MatrixDom]](val arg1: T1, val arg2: T2) extends ComposedDoubleTerm {
  self =>


  type ArgumentType = Term[MatrixDom]

  val arguments = IndexedSeq(arg1, arg2)


  def copy(args: IndexedSeq[ArgumentType]) = new MatrixDotProduct(args(0), args(1))

}

class VectorScaling[T1 <: VectorTerm, T2 <: DoubleTerm](val arg1: T1, arg2: T2) extends Composed[GenericVectorDom] {

  self =>

  type ArgumentType = Term[Dom]

  val arguments = IndexedSeq(arg1, arg2)

  def copy(args: IndexedSeq[ArgumentType]) =
    new VectorScaling(args(0).asInstanceOf[VectorTerm], args(1).asInstanceOf[DoubleTerm])

  override val domain = arg1.domain
}

class VectorConcatenation[T1 <: VectorTerm, T2 <: VectorTerm](val arg1: T1, val arg2: T2) extends Composed[GenericVectorDom] {

  self =>

  type ArgumentType = VectorTerm

  val arguments = IndexedSeq(arg1, arg2)

  def copy(args: IndexedSeq[ArgumentType]) = new VectorConcatenation(args(0), args(1))

  override val domain: VectorDom = new VectorDom(arg1.domain.dim + arg2.domain.dim)

}

class MatrixVectorProduct[T1 <: MatrixTerm, T2 <: VectorTerm](val arg1: T1, val arg2: T2) extends Composed[VectorDom] {

  self =>

  import ml.wolfe.util.PimpMyFactorie._


  type ArgumentType = Term[Dom]

  val arguments = IndexedSeq(arg1, arg2)

  override val domain = new VectorDom(arg1.domain.dim1)


  def copy(args: IndexedSeq[ArgumentType]) =
    new MatrixVectorProduct(args(0).asInstanceOf[MatrixTerm], args(1).asInstanceOf[VectorTerm])


  override def composer(args: Settings) = new Composer(args) {
    def eval()(implicit execution: Execution) = {
      output.vect(0) = input(0).mats(0) * input(1).vect(0)
    }
  }


  override def differentiatorImpl(wrt: Seq[AnyVar])(in: Settings, err: Setting, gradientAcc: Settings) =
    new ComposedDifferentiator(wrt, in, err, gradientAcc) {

      val diffArg1 = wrt.exists(arg1.vars.contains)
      val diffArg2 = wrt.exists(arg2.vars.contains)


      def localBackProp()(implicit execution: Execution) = {
        val A = argOutputs(0).mats(0)
        val x = argOutputs(1).vect(0)
        val errorVec = error.vect(0)
        require(A.dim2 == x.dim, s"dimensions don't match: ${A.dim1.toString + "x" +A.dim2.toString } * ${x.dim}")
        require(A.dim1 == errorVec.dim, s"dimensions don't match: ${A.dim1.toString + "x" +A.dim2.toString} * ${x.dim} => ${errorVec.dim}")

        //

        //argErrors(0).mats(0) := 0.0
        //argErrors(1).vect(0) := 0.0
//        if (argErrors(0).mats(0) == null) {
//          argErrors(0).mats(0) = new DenseTensor2(A.dim1,A.dim2)
//        }
//        for (i <- 0 until A.dim1 optimized; j <- 0 until A.dim2 optimized) {
//          argErrors(0).mats(0)
//        }


        if (diffArg1) argErrors(0).mats(0) = (errorVec outer x).asInstanceOf[ml.wolfe.Mat2]
        if (diffArg2) argErrors(1).vect(0) = A.t * errorVec

      }
    }

}

