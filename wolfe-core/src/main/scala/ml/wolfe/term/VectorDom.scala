package ml.wolfe.term

import cc.factorie.DenseTensor1
import cc.factorie.la.{SparseIndexedTensor1, SparseIndexedTensor, SparseHashTensor1, GrowableDenseTensor1}
import ml.wolfe._

/**
 * @author riedel
 */
class VectorDom(val dim: Int) extends GenericVectorDom {
  def one = new DenseTensor1(dim, 1.0)

  def zero = new DenseTensor1(dim, 0.0)

  override def sparseZero = new SparseIndexedTensor1(dim)

  def Term(values: Double*) = {
    require(values.size == dim)
    Const(new DenseVector(values.toArray))
  }
}

class VectorApply(vect: VectorTerm, index: IntTerm) extends ComposedDoubleTerm {
  type ArgumentType = AnyTerm

  val arguments = IndexedSeq(vect, index)

  def copy(args: IndexedSeq[ArgumentType]) =
    new VectorApply(args(0).asInstanceOf[VectorTerm], args(1).asInstanceOf[IntTerm])

  override def composer(args: Settings) = new Composer(args) {
    def eval()(implicit execution: Execution) = {
      val index = input(1).disc(0)
      output.cont(0) = input(0).vect(0)(index)
    }
  }
}

trait GenericVectorDom extends AtomicDom {
  dom =>

  def dim: Int

  type Value = Vect
  type Var = DomVar
  type Marginals = Vect

  val lengths = Offsets(0, 0, 1, 0)

  def toValue(setting: Setting, offsets: Offsets) =
    setting.vect(offsets.vectOff)


  def toMarginals(msg: Msg, offsets: Offsets) = {
    msg.vect(offsets.vectOff).mean
  }

  def fillZeroMsg(target: Msg, offsets: Offsets) = {
    target.vect(offsets.vectOff) = new VectMsg(one)
  }

  def copyValue(value: Value, setting: Setting, offsets: Offsets) =
    setting.vect(offsets.vectOff) = value


  def copyMarginals(marginals: Marginals, msgs: Msg, offsets: Offsets) = {
    msgs.vect(offsets.vectOff) = new VectMsg(marginals)
  }


  def Variable(name: String) = StaticVectorVar(name)

  case class StaticVectorVar(varName: String) extends DomTerm with DomVar


}

class TypedVectorDom[+D <: Dom](val argDom:D, val indexer:Indexer = new CanonicalIndexer) extends GenericVectorDom {

  require(argDom.isDiscrete)

  def dim = argDom.domainSize
  def one = ???
  def zero = new SparseHashTensor1(dim)
}

class GrowableVectorDom(val dim: Int) extends GenericVectorDom {
  def one = {
    val result = new GrowableDenseTensor1(dim)
    result := 1.0
    result
  }

  def zero = new GrowableDenseTensor1(dim)

  def Term(values: Double*) = {
    require(values.size == dim)
    val result = new GrowableDenseTensor1(values.size)
    result := values.toArray
    Const(result)
  }

}


