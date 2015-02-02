package ml.wolfe.term

import cc.factorie.Factorie.DenseTensor1
import ml.wolfe.FactorieVector
import scala.language.implicitConversions

/**
 * @author riedel
 */
object TermImplicits {

  val doubles = Dom.doubles
  val bools = Dom.bools

  def vectors(dim: Int) = new VectorDom(dim)

  def discrete[T](args: T*) = new DiscreteDom[T](args.toIndexedSeq)

  def vector(values: Double*) = new DenseTensor1(values.toArray)

  def seqs[D <: Dom](elements: D, length: Int) = new SeqDom(elements, length)

  def sigm[T <: DoubleTerm](term: T) = new Sigmoid(term)

  def log[T <: DoubleTerm](term: T) = new Log(term)

  def I[T <: BoolTerm](term: T) = new Iverson(term)

  implicit def doubleToConstant(d: Double): Constant[DoubleDom] = new Constant[DoubleDom](Dom.doubles, d)

  implicit def vectToConstant(d: FactorieVector): Constant[VectorDom] = new Constant[VectorDom](vectors(d.dim1), d)

  implicit def discToConstant[T: DiscreteDom](value: T): Constant[DiscreteDom[T]] =
    new Constant[DiscreteDom[T]](implicitly[DiscreteDom[T]], value)


  def argmax[D <: Dom](dom: D)(obj: dom.Variable => DoubleTerm): dom.Value = {
    val variable = dom.variable("_hidden")
    val term = obj(variable)
    term.argmax(variable).asInstanceOf[dom.Value]
  }

  def max[D <: Dom](dom: D)(obj: dom.Variable => DoubleTerm) = {
    val variable = dom.variable("_hidden")
    val term = obj(variable)
    new Max(term, Seq(variable))
  }

  implicit class RichDoubleTerm(term: DoubleTerm) {
    def +(that: DoubleTerm) = new Sum(IndexedSeq(term, that))

    def *(that: DoubleTerm): Product = new Product(IndexedSeq(term, that))
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
    def apply(args: Any*) = term.apply(args)
  }

  implicit class RichDom[D <: Dom](val dom: D) {
    def x[D2 <: Dom](that: D2) = new Tuple2Dom[D, D2](dom, that)
  }

  implicit class RichVectTerm(val vect: Term[VectorDom]) {
    def dot(that: Term[VectorDom]) = new DotProduct(vect, that)
  }

}
