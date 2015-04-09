package ml.wolfe.term

import cc.factorie.la._
import cc.factorie.util.SingletonIntSeq
import ml.wolfe.{SimpleIndex, Index}

/**
 * @author riedel
 */
case class OneHot(index: IntTerm, value: DoubleTerm)(implicit val domain: VectorDom) extends Composed[VectorDom] {

  type ArgumentType = AnyTerm
  val arguments = IndexedSeq(index, value)


  override def composer(args: Settings) = new Composer(args) {
    output.vect(0) = new SparseTensor1(domain.dim)

    val result = output.vect(0)

    def eval()(implicit execution: Execution) = {
      val index = input(0).disc(0)
      val value = input(1).cont(0)
      //result.move(index,value)
      result.zero()
      result(index) = value
      output.vect.recordChange(0)
    }
  }


  override def differentiatorImpl(wrt: Seq[AnyVar])(in: Settings, err: Setting, gradientAcc: Settings) =
    new ComposedDifferentiator(wrt,in,err,gradientAcc) {
      def localBackProp()(implicit execution: Execution) = {
        val index = argOutputs(0).disc(0)
        argErrors(0).disc(0) = index //todo: this is nasty, needs to be fixed
        argErrors(1).cont(0) = error.vect(0)(index)
      }
    }

  def copy(args: IndexedSeq[ArgumentType]) =
    OneHot(args(0).asInstanceOf[IntTerm], args(1).asInstanceOf[DoubleTerm])(domain)
}

/** A one-dimensional one-hot Tensor. */
class MutableSingletonTensor1(val dim1:Int, var singleIndex:Int, var singleValue:Double) extends SingletonIndexedTensor with Tensor1 {
  def activeDomain = new SingletonIntSeq(singleIndex)

  def move(i:Int, v:Double): Unit = {
    singleIndex = i
    singleValue = v
  }

}

trait Indexer {
  def index(dom:Dom,setting:Setting):Int
}

class DefaultIndexer(val index:Index = new SimpleIndex) extends Indexer {
  def index(dom: Dom, setting: Setting) = {
    val value = Seq() ++ setting.disc.array
    val indexOfValue = index(value)
    indexOfValue
  }
}

case class Indexed[T <: Term[Dom]](arg: T)(implicit val indexer: Indexer) extends Composed[IntDom] {

  require(arg.domain.isDiscrete, "can only index discrete terms")

  type ArgumentType = T

  def copy(args: IndexedSeq[ArgumentType]) = new Indexed[T](args(0))(indexer)

  val arguments = IndexedSeq(arg)
  val domain = Dom.ints

  override def composer(args: Settings) = new Composer(args) {
    def eval()(implicit execution: Execution) = {
      val indexOfValue = indexer.index(arg.domain, input(0))
      output.disc(0) = indexOfValue
    }
  }
}

case class Conjoined(arg1:VectorTerm,arg2:VectorTerm)(implicit val index:Index,val dom:VectorDom) extends Composed[VectorDom] {
  type ArgumentType = VectorTerm
  val domain = dom
  val arguments = IndexedSeq(arg1,arg2)

  def copy(args: IndexedSeq[ArgumentType]) = Conjoined(args(0),args(1))(index,dom)

  override def composer(args: Settings) = new Composer(args) {
    output.vect(0) = new GrowableSparseTensor1(0 until 100)
//    output.vect(0) = new SparseIndexedTensor1(1000)

    def eval()(implicit execution: Execution) = {
      val a1 = input(0).vect(0)
      val a2 = input(1).vect(0)
      output.vect(0).zero()
      for ((i1,v1) <- a1.activeElements; (i2,v2) <- a2.activeElements) {
        val i = index(i1 -> i2)
        output.vect(0)(i) = v1 * v2
      }
    }
  }
}