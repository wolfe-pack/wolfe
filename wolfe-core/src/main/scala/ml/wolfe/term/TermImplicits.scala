package ml.wolfe.term

import cc.factorie.Factorie.DenseTensor1
import cc.factorie.la.{SingletonTensor1, DenseTensor2}
import ml.wolfe.{FactorieMatrix, FactorieVector}
import scala.language.implicitConversions

/**
 * @author riedel
 */
object TermImplicits {

  implicit val doubles = Dom.doubles
  implicit val bools = Dom.bools

  def vectors(dim: Int) = new VectorDom(dim)

  def matrices(dim1: Int, dim2: Int) = new MatrixDom(dim1: Int, dim2: Int)

  def discrete[T](args: T*) = new DiscreteDom[T](args.toIndexedSeq)

  def vector(values: Double*) = new DenseTensor1(values.toArray)

  def matrix(values: Seq[Double]*) = {
    val tmp = new DenseTensor2(values.length, values.head.length)

    (0 until values.length).foreach(row => {
      (0 until values(row).length).foreach(col => {
        tmp(row, col) = values(row)(col)
      })
    })

    tmp
  }

  implicit def domToIterable(dom: Dom): Iterable[dom.Value] = dom.toIterable

  def seqs[D <: Dom](elements: D, length: Int): SeqDom[elements.type] = new SeqDom[elements.type](elements, length)

  def seq[E <: Dom](dom: SeqDom[E])(elems: dom.elementDom.Term*): dom.SeqDomTermImpl = new dom.SeqDomTermImpl {
    def elements = elems.toIndexedSeq

    def copy(args: IndexedSeq[ArgumentType]) = seq(dom)(args: _*)
  }

  def sigm[T <: DoubleTerm](term: T) = new Sigmoid(term)

  def tanh[T <: DoubleTerm](term: T) = new Tanh(term)

  def log[T <: DoubleTerm](term: T) = new Log(term)

  def I[T <: BoolTerm](term: T) = new Iverson(term)

  def sigmVec[T <: VectorTerm](term: T) = new VectorSigmoid(term)

  def tanhVec[T <: VectorTerm](term: T) = new VectorTanh(term)

  //  implicit def genericToConstant[T,D<:TypedDom[T]](t:T)(implicit dom:D):dom.Term = dom.const(t)
  //  implicit def genericToConstant[T,D<:TypedDom[T]](t:T)(implicit dom:D):dom.DomTerm = dom.const(t)

  //  implicit def seqOfTermsToSeqTerm[T <: Term[Dom], D <: DomWithTerm[T],S<:SeqDom[D]](seq:IndexedSeq[T])(implicit dom:S):dom.Term =
  //    new dom.SeqTermImpl {
  //      val elements:IndexedSeq[T] = seq
  //      val domain:dom.type = dom
  //    }

  def sequential2[T](seq: IndexedSeq[T]) = new Generator[T] {
    private var _current = -1

    def generateNext() = {
      _current = (_current + 1) % seq.length
    }

    def current() = seq(_current)

    def termsPerEpoch: Int = seq.length
  }


  def stochastic[T](gen: Dynamic[T])(arg: Dynamic[T] => DoubleTerm) = {
    val term = arg(gen)
    new DynamicTerm[DoubleDom, T] {
      def generator: Dynamic[T] = gen
      def self: Term[DoubleDom] = term
    }
  }


  //implicit def seqToConstant[T,D<:TypedDom[T]](seq:IndexedSeq[T])(implicit dom:SeqDom[D]):dom.TermType = dom.const(seq)

  //implicit def seqToSeqTerm[E <: Dom : SeqDom](elems:Seq[Term[E]]) = seq(implicitly[SeqDom[E]])(elems: _*)

  implicit def doubleToConstant(d: Double): Constant[DoubleDom] = Dom.doubles.const(d)

  implicit def intToDoubleConstant(d: Int): Constant[DoubleDom] = Dom.doubles.const(d)

  implicit def vectToConstant(d: FactorieVector): Constant[VectorDom] = vectors(d.dim1).const(d)

  implicit def vectToConstantWithDom(d: FactorieVector)(implicit dom: VectorDom): dom.Term = dom.const(d)
  implicit def dynVectToConstantWithDom(d: Dynamic[FactorieVector])(implicit dom: VectorDom): dom.Term = dom.dynConst(d)

  implicit def matToConstant(d: FactorieMatrix): Constant[MatrixDom] = matrices(d.dim1, d.dim2).const(d)

  implicit def discToConstant[T: DiscreteDom](value: T): Constant[DiscreteDom[T]] =
    implicitly[DiscreteDom[T]].const(value)


  //  def argmax[D <: Dom](dom: D)(obj: dom.Variable => DoubleTerm): dom.Value = {
  //    val variable = dom.variable("_hidden")
  //    val term = obj(variable)
  //    term.argmax(variable).asInstanceOf[dom.Value]
  //  }

  def argmax[D <: Dom](dom: D)(obj: dom.Var => DoubleTerm): Argmax[dom.type] = {
    val variable = dom.variable("_hidden")
    val term = obj(variable)
    new Argmax[dom.type](term, variable)
  }


  def max[D <: Dom](dom: D)(obj: dom.Var => DoubleTerm) = {
    val variable = dom.variable("_hidden")
    val term = obj(variable)
    new Max(term, Seq(variable))
  }

  def sum[T](dom: Seq[T])(arg: T => DoubleTerm) = new Sum(dom.toIndexedSeq.map(arg))

  def sum(args: DoubleTerm*) = new Sum(args.toIndexedSeq)

  def oneHot(index: Int, value: Double = 1.0)(implicit dom: VectorDom) =
    dom.const(new SingletonTensor1(dom.dim, index, value))

  implicit class RichDoubleTerm(term: DoubleTerm) {
    def +(that: DoubleTerm) = new Sum(IndexedSeq(term, that))

    def -(that: DoubleTerm) = new Sum(IndexedSeq(term, that * (-1.0)))

    def *(that: DoubleTerm): Product = new Product(IndexedSeq(term, that))

    def *(that: VectorTerm) = new VectorScaling(that, term)

    def argmaxBy(factory: ArgmaxerFactory) = new ProxyTerm[DoubleDom] {
      def self = term

      override def argmaxer(wrt: Seq[Var[Dom]]) = factory.argmaxer(term, wrt)
    }
  }

  implicit class RichBoolTerm(term: BoolTerm) {
    def &&(that: BoolTerm) = new And(term, that)

    def ||(that: BoolTerm) = new Or(term, that)

    def -->(that: BoolTerm) = new Implies(term, that)
  }

  implicit class RichDiscreteTerm[T](term: DiscreteTerm[T]) {
    def ===(that: DiscreteTerm[T]) = new DiscreteEquals(term, that)
  }

  implicit class RichTerm[D <: Dom](val term: Term[D]) {
    def typed[A <: Dom](dom: A): dom.Value => term.domain.Value = {
      require(term.vars.size == 1 && term.vars.head.domain == dom)
      (a: dom.Value) => term.eval(Seq(a): _*)
    }
  }

  implicit class RichDom[D <: Dom](val dom: D) {
    def x[D2 <: Dom](that: D2): Tuple2Dom[dom.type, that.type] = new Tuple2Dom[dom.type, that.type](dom, that)

    //    def iterator = dom.iterator
  }

  implicit class RichVectTerm(val vect: Term[VectorDom]) {
    def dot(that: Term[VectorDom]) = new DotProduct(vect, that)

    def *(that: Term[DoubleDom]) = new VectorScaling(vect, that)

    //element-wise addition
    def :+(that: Term[VectorDom]): VectorTerm = ???

    //element-wise multiplication
    def :*(that: Term[VectorDom]): VectorTerm = ???

    def cons(that: Term[VectorDom]): VectorTerm = new VectorConcatenation(vect, that)
  }

  implicit class RichMatrixTerm(val mat: Term[MatrixDom]) {
    def dot(that: Term[MatrixDom]) = new MatrixDotProduct(mat, that)

    def *(that: Term[VectorDom]) = new MatrixVectorProduct(mat, that)
  }

}
