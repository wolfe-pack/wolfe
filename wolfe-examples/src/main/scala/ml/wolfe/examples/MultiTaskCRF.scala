package ml.wolfe.examples

import ml.wolfe.SimpleIndex
import ml.wolfe.term.LearningObjective._
import ml.wolfe.term.TermImplicits._
import ml.wolfe.term._
import ml.wolfe._
import ml.wolfe.nlp.{SentenceSplitter, TokenSplitter}

/**
 * @author sameer
 * @since 6/6/15.
 */
object MultiTaskCRF {

  object Data {
    // determine from data
    val doc = TokenSplitter(SentenceSplitter(
      "John Denver is a Songwriter. Denver has produced many records"))
    val words = doc.tokens.map(_.word).distinct
    val tags = Seq("O", "B-LOC", "I-LOC", "B-PER", "I-PER")
    val maxLength = 100

    // number of labels
    val L = tags.length
    // number of unary features
    val N = L + words.length

    // data domains
    implicit val Words = words.toDom withOOV "[OOV]"
    implicit val Tags = tags.toDom
    implicit val Y = Seqs(Tags, 0, maxLength)
    implicit val X = Seqs(Words, 0, maxLength)
    implicit val Instances = Pairs(X, Y)

    implicit val index = new SimpleIndex()
    val data = IndexedSeq.empty[(IndexedSeq[String], IndexedSeq[String])]
  }

  trait Model {
    def predict : IndexedSeq[String] => IndexedSeq[String]
  }

  object CRFModel extends Model {

    import Data._

    // model domains
    @domain case class Theta(w: Vect)

    implicit val Features = Vectors(N * L + L * L)
    implicit val Thetas = Theta.Values(Features)
    implicit val maxProductParams = BPParameters(iterations = 2)

    def model(w: Thetas.Term)(x: X.Term)(y: Y.Term) = {
      sum(0 until x.length) { i => w.w dot feature('bias, y(i))} +
        sum(0 until x.length) { i => w.w dot feature('word, y(i) -> x(i))} +
        sum(0 until x.length - 1) { i => w.w dot feature('pair, y(i) -> y(i + 1))}
    } subjectTo (y.length === x.length)

    val params = AdaGradParameters(10, 0.1)
    lazy val thetaStar =
      learn(Thetas)(t => perceptron(data.toConst)(Y)(model(t))) using Argmaxer.adaGrad(params)

    val predict = fun(X) { x => argmax(Y)(model(Thetas.Const(thetaStar))(x))}
  }

  object MultiTaskModel extends Model {

    import Data._

    val k = 10

    // model domains
    @domain case class Theta(a: Mat, w: Vect)

    implicit val Features = Vectors(N + L * L)
    implicit val Thetas = Theta.Values(Matrices(L * k, N), Features)
    implicit val maxProductParams = BPParameters(iterations = 2)

    def model(w: Thetas.Term)(x: X.Term)(y: Y.Term) = {
      sum(0 until x.length) { i => w.w dot (w.a * feature('bias, y(i)))} +
        sum(0 until x.length) { i => w.w dot (w.a * feature('word, y(i) -> x(i)))} +
        sum(0 until x.length - 1) { i => w.w dot feature('pair, y(i) -> y(i + 1))}
    } subjectTo (y.length === x.length)

    val params = AdaGradParameters(10, 0.1)
    lazy val thetaStar =
      learn(Thetas)(t => perceptron(data.toConst)(Y)(model(t))) using Argmaxer.adaGrad(params)

    val predict = fun(X) { x => argmax(Y)(model(Thetas.Const(thetaStar))(x))}
  }

  object NeuralModel extends Model {

    import Data._

    val ak = 10
    val bk = 10

    // model domains
    @domain case class Theta(a: Mat, b: Mat, w: Vect)

    implicit val Features = Vectors(N + L * L)
    implicit val Thetas = Theta.Values(Matrices(L * ak, bk), Matrices(bk, N), Features)
    implicit val maxProductParams = BPParameters(iterations = 2)

    def model(w: Thetas.Term)(x: X.Term)(y: Y.Term) = {
      sum(0 until x.length) { i => w.w dot (w.a * sigmVec(w.b * feature('bias, y(i))))} +
        sum(0 until x.length) { i => w.w dot (w.a * sigmVec(w.b * feature('word, y(i) -> x(i))))} +
        sum(0 until x.length - 1) { i => w.w dot feature('pair, y(i) -> y(i + 1))}
    } subjectTo (y.length === x.length)

    val params = AdaGradParameters(10, 0.1)
    lazy val thetaStar =
      learn(Thetas)(t => perceptron(data.toConst)(Y)(model(t))) using Argmaxer.adaGrad(params)

    val predict = fun(X) { x => argmax(Y)(model(Thetas.Const(thetaStar))(x))}
  }

}
