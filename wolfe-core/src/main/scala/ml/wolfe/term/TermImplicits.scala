package ml.wolfe.term

import cc.factorie.Factorie.DenseTensor1
import cc.factorie.la.{SingletonTensor1, DenseTensor2}
import ml.wolfe.{FactorieMatrix, FactorieVector}
import scala.language.implicitConversions

/**
 * @author riedel
 */
object TermImplicits extends NameProviderImplicits {

  implicit val doubles:DoubleDom = Dom.doubles
  implicit val bools = Dom.bools

  def vectors(dim: Int) = new VectorDom(dim)

  def unitVectors(dim: Int) = new UnitVectorDom(dim)

  def matrices(dim1: Int, dim2: Int) = new MatrixDom(dim1: Int, dim2: Int)

  def discrete[T](args: T*) = new DiscreteDom[T](args.toIndexedSeq)

  def dom[T](args: Seq[T]) = new DiscreteDom[T](args.toIndexedSeq)

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

  def seqs[D <: Dom](elements: D, minLength: Int, maxLength:Int): VarSeqDom[elements.type] =
    new VarSeqDom[elements.type](elements, maxLength, minLength)


  def seq[E <: Dom](dom: SeqDom[E])(elems: dom.elementDom.Term*): dom.SeqDomTermImpl = new dom.SeqDomTermImpl {
    def elements = elems.toIndexedSeq

    def copy(args: IndexedSeq[ArgumentType]) = seq(dom)(args: _*)
  }

  def sigm[T <: DoubleTerm](term: T) = new Sigmoid(term)

  def sqrt[T <: DoubleTerm](term: T) = new Sqrt(term)

  def clip[T <: DoubleTerm](term: T) = new Clip(term)

  def tanh[T <: DoubleTerm](term: T) = new Tanh(term)

  def log[T <: DoubleTerm](term: T) = new Log(term)

  def I[T <: BoolTerm](term: T) = new Iverson(term)

  def sigmVec[T <: VectorTerm](term: T) = new VectorSigmoid(term)

  def tanhVec[T <: VectorTerm](term: T) = new VectorTanh(term)

  def l1[T <: VectorTerm](term: T) = new L1Norm[term.type](term)
  def l2[T <: VectorTerm](term: T) = sqrt(term dot term)

  //  implicit def genericToConstant[T,D<:TypedDom[T]](t:T)(implicit dom:D):dom.Term = dom.const(t)
  //  implicit def genericToConstant[T,D<:TypedDom[T]](t:T)(implicit dom:D):dom.DomTerm = dom.const(t)

  //  implicit def seqOfTermsToSeqTerm[T <: Term[Dom], D <: DomWithTerm[T],S<:SeqDom[D]](seq:IndexedSeq[T])(implicit dom:S):dom.Term =
  //    new dom.SeqTermImpl {
  //      val elements:IndexedSeq[T] = seq
  //      val domain:dom.type = dom
  //    }

  def VarSeq[D<:Dom](length:TypedTerm[Int],args:IndexedSeq[Term[D]]) = {
    new VarSeqConstructor[D](length,args)
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

  implicit def intToDouble(int:DiscreteTerm[Int]):IntToDouble[int.type] = new IntToDouble[int.type](int)

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

  def sum[T <: Term[VarSeqDom[DoubleDom]]](args: T) = new VarSeqSum[DoubleDom,T](args)

  def sum[T <: DoubleTerm, D <: Dom](d:D)(body:d.Var => T) = {
    val variable = d.variable("_i")
    val bodyTerm = body(variable)
    new FirstOrderSum[d.type,T](variable,bodyTerm)
  }

  def oneHot(index: Int, value: Double = 1.0)(implicit dom: VectorDom) =
    dom.const(new SingletonTensor1(dom.dim, index, value))

  implicit class RichDoubleTerm(term: DoubleTerm) {
    def +(that: DoubleTerm) = new Sum(IndexedSeq(term, that))

    def -(that: DoubleTerm) = new Sum(IndexedSeq(term, that * (-1.0)))

    def *(that: DoubleTerm): Product = new Product(IndexedSeq(term, that))

    def *(that: VectorTerm) = new VectorScaling(that, term)

    def /(that: DoubleTerm) = new Div(term, that)

    def unary_- = term * (-1.0)

    def argmaxBy(factory: ArgmaxerFactory) = new ProxyTerm[DoubleDom] {
      def self = term

      override def argmaxer(wrt: Seq[Var[Dom]]) = factory.argmaxer(term, wrt)
    }
  }

  implicit class RichBoolTerm(term: BoolTerm) {
    def &&(that: BoolTerm) = new And(term, that)

    def ||(that: BoolTerm) = new Or(term, that)

    def -->(that: BoolTerm) = new Implies(term, that)

    def unary_! = for (b <- term ) yield !b

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

//  implicit class RichSeqTerm[E <: Dom, S <: Term[SeqDom[E]]](val term:S) {
//    def apply[I <: Term[TypedDom[Int]]](index:I) =
//      new SeqApply[E,S,I](term,index)
//  }

  implicit class RichMonadTerm[A <: Term[Dom]](val termToBeMapped: A) {
    def map[B: TypedDom](fun: termToBeMapped.domain.Value => B) = {
      val targetDom = implicitly[TypedDom[B]]
      new TermMap[A, targetDom.type] {
        val term: termToBeMapped.type = termToBeMapped
        val domain: targetDom.type = targetDom

        def f(arg: term.domain.Value) = fun(arg)
      }
    }

    def flatMap[B: TypedDom](fun: termToBeMapped.domain.Value => Term[TypedDom[B]]) = {
      val targetDom = implicitly[TypedDom[B]]
      new TermFlatMap[A, TypedDom[B]] {
        val term: termToBeMapped.type = termToBeMapped
        val domain: TypedDom[B] = targetDom

        def f(arg: term.domain.Value): Term[TypedDom[B]] = fun(arg)
      }
    }
  }


  implicit class RichDom[D <: Dom](val dom: D) {
    def x[D2 <: Dom](that: D2): Tuple2Dom[dom.type, that.type] = new Tuple2Dom[dom.type, that.type](dom, that)
    //    def iterator = dom.iterator
  }


  class RichVarSeqTerm[E <: Dom, T <: Term[VarSeqDom[E]]](val term:T) {
    def apply(index:Int) =
      new VarSeqApply[E,T,term.domain.lengthDom.Term](term,term.domain.lengthDom.const(index))
    def length = new VarSeqLength[T](term)
    //def apply(index:Int) = term.elements(index)

  }

  implicit class RichVarSeqDom[E <: Dom](val dom:VarSeqDom[E]) {
//    def Term2(length:TypedTerm[Int],elements:IndexedSeq[Term[E]]):dom.Term = new dom.Constructor(length,elements)

  }

//  implicit def toRichVarSeqTerm[S <:Term[VarSeqDom[_]]](seq:S):RichVarSeqTerm[seq.domain.ElemDom,seq.type] = {
//    ???
//  }

  implicit class RichVectTerm(val vect: VectorTerm) {
    def dot(that: VectorTerm) = new DotProduct(vect, that)

    def *(that: Term[DoubleDom]) = new VectorScaling(vect, that)

    def +(that: VectorTerm) = new VectorSum(IndexedSeq(vect, that))

    def -(that: VectorTerm) = new VectorSum(IndexedSeq(vect, that * (-1.0)))


    //element-wise addition
    def :+(that: VectorTerm): VectorTerm = ???

    //element-wise multiplication
    def :*(that: VectorTerm): VectorTerm = ???

    def cons(that: VectorTerm): VectorTerm = new VectorConcatenation(vect, that)

    def l2(mask: VectorTerm = null): DoubleTerm = new SparseL2(vect, mask)
  }

  implicit class RichMatrixTerm(val mat: Term[MatrixDom]) {
    def dot(that: Term[MatrixDom]) = new MatrixDotProduct(mat, that)

    def *(that: VectorTerm) = new MatrixVectorProduct(mat, that)
  }

//  implicit class VarCreator[D<:Dom](val d:D) {
//    def Var(implicit provider:NameProvider):d.Var = d.variable(provider.newName())
//  }

}
