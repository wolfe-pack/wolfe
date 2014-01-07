package scalapplcodefest.example

import scalapplcodefest.Wolfe

/**
 * @author Sebastian Riedel
 */
object LDAExample {

  import Wolfe._
  import math._

  case class Token(z: Int, w: String)
  case class Doc(theta: Seq[Double], tokens: Seq[Token])
  case class Data(docs: Seq[Doc], phis: Seq[Map[String, Double]])

  val topics = Range(0, 10).toSet
  val vocab = Seq("the", "cat")

  def lda(params: Vector)(data: Data): Double = {
    implicit val weights = params
    import data._
    //dirichlet prior for all topics
    //using generators
    story(
      allIn(topics) {t => phis(t) ~ Dirichlet(hyper('alpha -> t))},
      allIn(docs) {d => d.theta ~ Dirichlet(hyper('theta -> d))},
      allIn(docs) {d => allIn(d.tokens) {t => t.z ~ Multinomial(d.theta)}},
      allIn(docs) {d => allIn(d.tokens) {t => t.w ~ Multinomial(phis(t.z))}}
    )
  }

  case class Test(x: Boolean, y: Boolean)

  val test = c(bools, bools) map Test

  def story(stats: Double*) = sum(stats)(identity)

  def allIn[T](elems: Seq[T])(stats: T => Double) = sum(elems)(stats)

  def hyper(id: Any)(implicit w: Vector) = (v: Any) => w dot ft(id -> v, 1.0)

  trait Distribution[T] {
    def score(value: T): Double
  }

  trait MultivariateDistribution[T] {
    def score(value: T, index: Any): Double
  }

  case class Dirichlet(alpha: Any => Double) extends MultivariateDistribution[Double] {
    //Multidimensional
    def score(value: Double, index: Any) = alpha(index) * log(value)
  }


  case class Multinomial[T](prior: T => Double) extends Distribution[T] {
    def score(value: T) = log(prior(value))
  }


  implicit class GeneratedSeq[T](seq: Seq[T]) {
    def ~(generator: MultivariateDistribution[T]) = {
      sum(seq.indices) {i => generator.score(seq(i), i)}
    }
  }

  implicit class GeneratedMap[K, T](seq: Map[K, T]) {
    def ~(generator: MultivariateDistribution[T]) = {
      sum(seq.keys.toSeq) {i => generator.score(seq(i), i)}
    }
  }

  implicit class Generated[T](value: T) {
    def ~(generator: Distribution[T]) = generator.score(value)
  }


}
