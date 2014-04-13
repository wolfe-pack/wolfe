package ml.wolfe.examples

import ml.wolfe.Wolfe

/**
 * @author Sebastian Riedel
 */
object LDAExample {

  import Wolfe._

  case class Token(topic: Int, word: String)
  case class Doc(theta: Map[Int, Double], tokens: Seq[Token])
  case class Data(phi: Seq[Map[String, Double]], docs: Seq[Doc])

  def prod[T](dom: Iterable[T])(p: T => Double) = dom.map(p).product
  def gen[T](dom: Iterable[T])(p: T => Double) = prod(dom)(p)


  def dir[T](dist: Map[T, Double], beta: Double): Double = 0.0
  def cat[T](c: T, dist: Map[T, Double]): Double = 0.0

  trait Dist[T] {
    def apply(t: T): Double = 0.0
  }
  implicit class RV[T](t: T) {
    def ~(dist: Dist[T]) = dist(t)
  }
  case class Cat[T](dist: Map[T, Double]) extends Dist[T]
  case class Dir[T](beta: Double) extends Dist[Map[T, Double]]

  def lda(w: Vector)(d: Data) = {
    import d._
    prod(phi) { dir(_, w('beta)) } *
    prod(docs) {
      d =>
        dir(d.theta, w('alpha)) *
        prod(d.tokens) {
          t =>
            cat(t.topic, d.theta) *
            cat(t.word, phi(t.topic))
        }
    }

  }

  def lda2(w: Vector)(d: Data) = {
    import d._
    gen(phi) { _ ~ Dir(w('beta)) } *
    gen(docs)(d => d.theta ~ Dir(w('alpha)) * gen(d.tokens)(t => t.topic ~ Cat(d.theta) * t.word ~ Cat(phi(t.topic))))
  }

  implicit class DoubleWithAnd(d: Double) {
    def and(that: Double) = d * that
  }

  def lda3(w: Vector)(d: Data) = {
    import d._
    gen(phi) { _ ~ Dir(w('beta)) } and
    gen(docs) {
      d =>
        d.theta ~ Dir(w('alpha)) * gen(d.tokens) {
          t =>
            t.topic ~ Cat(d.theta) and
            t.word ~ Cat(phi(t.topic))
        }
    }
  }


}
