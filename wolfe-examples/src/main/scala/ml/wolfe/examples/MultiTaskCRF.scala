package ml.wolfe.examples

import ml.wolfe.SimpleIndex
import ml.wolfe.nlp.io.CoNLLReader
import ml.wolfe.term.LearningObjective._
import ml.wolfe.term.TermImplicits._
import ml.wolfe.term._
import ml.wolfe._
import ml.wolfe.nlp.{Sentence, SentenceSplitter, TokenSplitter}
import ml.wolfe.util.Math.random

import scala.util.Random

/**
 * @author sameer
 * @since 6/6/15.
 */
object MultiTaskCRF extends App {

  implicit val random = new Random(0)

  object Data {
    val connl2000Train = new CoNLLReader("wolfe-examples/src/main/resources/ml/wolfe/datasets/conll2000/train.txt", " ")
    val connl2000Test = new CoNLLReader("wolfe-examples/src/main/resources/ml/wolfe/datasets/conll2000/test.txt", " ") //

    def getChunkTags(s: Sentence) = {
      val chunkTags = Array.fill(s.tokens.length)("O")
      s.ie.entityMentions.foreach { chunk =>
        chunkTags(chunk.start) = if (chunk.label == "O") "O" else "B-" + chunk.label
        for (i <- chunk.start + 1 until chunk.end) chunkTags(i) = "I-" + chunk.label
      }
      collection.immutable.IndexedSeq(chunkTags: _*)
    }

    def getPosTags(s: Sentence) = {
      val posTags = s.tokens.map(_.posTag)
      collection.immutable.IndexedSeq(posTags: _*)
    }

    def toInstance(s: Sentence): (IndexedSeq[String], IndexedSeq[String]) = {
      //s.tokens.map(_.word.toString) -> getChunkTags(s)
      s.tokens.map(_.word.toString) -> getPosTags(s)
    }

    val train = connl2000Train.take(10).map(toInstance).toIndexedSeq
    val test = connl2000Test.take(1).map(toInstance).toIndexedSeq

    val words = (train ++ test).flatMap(_._1).distinct
    val tags = (train ++ test).flatMap(_._2).distinct
    val maxLength = (train ++ test).map(_._1.length).max

    println(s"train: ${train.length}, test: ${test.length}")
    println(s"words: ${words.length}, tags: ${tags.length}, maxLength: $maxLength")

    // number of labels
    val L = tags.length
    // number of unary features
    val N = words.length + 1

    // data domains
    implicit val Words = words.toDom withOOV "[OOV]"
    implicit val Tags = tags.toDom
    implicit val Y = Seqs(Tags, 0, maxLength)
    implicit val X = Seqs(Words, 0, maxLength)
    implicit val Instances = Pairs(X, Y)

    val numFeats = N * L + L
    implicit val Features = Vectors(numFeats)
    val TransitionFeats = Vectors(L * L)
  }

  trait Model {
    def predict: IndexedSeq[String] => IndexedSeq[String]

  }

  object CRFModel extends Model {


    import Data._

    // model domains
    @domain case class Theta(w: Vect, wb: Vect)

    implicit val Thetas = Theta.Values(Features, TransitionFeats)
    implicit val maxProductParams = BPParameters(iterations = 1, cachePotentials = true)

    val localIndex = new SimpleFeatureIndex(Features)
    val transIndex = new SimpleFeatureIndex(TransitionFeats)

    def local(x: X.Term, y: Y.Term, i: IntTerm) = {
      localIndex.oneHot('bias, y(i)) + localIndex.oneHot('word, y(i), x(i))
    }

    def transition(x: X.Term, y: Y.Term, i: IntTerm) = {
      transIndex.oneHot('pair, y(i), y(i + 1))
    }

    def model(w: Thetas.Term)(x: X.Term)(y: Y.Term) = {
      sum(0 until x.length) { i => w.w dot local(x, y, i) } +
        sum(0 until x.length - 1) { i => w.wb dot transition(x, y, i) }
    } subjectTo (y.length === x.length) argmaxBy Argmaxer.maxProduct

    val init = Settings(Thetas.createZeroSetting())
    def myEpochHook(epoch:Int, params:IndexedSeq[Any],obj:Double) = {
      val epochThetaStar = params.head.asInstanceOf[Theta]
      val epochPredict = fun(X) { x => argmax(Y)(model(Thetas.Const(epochThetaStar))(x)) }
      val error = run(epochPredict,test)
      error.toString
    }
    val params = AdaGradParameters(100, 0.1, 0.1, initParams = init, epochHook = myEpochHook)
    lazy val thetaStar =
      learn(Thetas)(t => perceptron(train.toConst)(Y)(model(t))) using Argmaxer.adaGrad(params)

    val predict = fun(X) { x => argmax(Y)(model(Thetas.Const(thetaStar))(x)) }
  }

  object MultiTaskModel extends Model {

    import Data._

    val k = 10

    // model domains
    @domain case class Theta(a: Mat, w: Vect, wb: Vect)

    implicit val Thetas = Theta.Values(Matrices(k, numFeats), Vectors(k), TransitionFeats)
    implicit val maxProductParams = BPParameters(iterations = 1)

    val localIndex = new SimpleFeatureIndex(Features)
    val transIndex = new SimpleFeatureIndex(TransitionFeats)

    def local(x: X.Term, y: Y.Term, i: IntTerm) = {
      localIndex.oneHot('bias, y(i)) + localIndex.oneHot('word, y(i), x(i))
    }

    def transition(x: X.Term, y: Y.Term, i: IntTerm) = {
      transIndex.oneHot('pair, y(i), y(i + 1))
    }

    def model(w: Thetas.Term)(x: X.Term)(y: Y.Term) = {
      sum(0 until x.length) { i => w.w dot (w.a * local(x, y, i)) } +
        sum(0 until x.length - 1) { i => w.wb dot transition(x, y, i) }
    } subjectTo (y.length === x.length) argmaxBy Argmaxer.maxProduct

    val init = Settings(Thetas.createRandomSetting(random.nextGaussian() * 0.1))

    def myEpochHook(epoch:Int, params:IndexedSeq[Any],obj:Double) = {
      val epochThetaStar = params.head.asInstanceOf[Theta]
      val epochPredict = fun(X) { x => argmax(Y)(model(Thetas.Const(epochThetaStar))(x)) }
      val error = run(epochPredict,test)
      error.toString
    }

    val params = AdaGradParameters(10, 0.1, 0.1, initParams = init, epochHook = null)
    lazy val thetaStar =
      learn(Thetas)(t => perceptron(train.toConst)(Y)(model(t))) using Argmaxer.adaGrad(params)

    val predict = fun(X) { x => argmax(Y)(model(Thetas.Const(thetaStar))(x)) }
  }

  object SingleLayerNeuralCRF extends Model {

    import Data._
    import Argmaxer._

    val localIndex = new SimpleFeatureIndex(Features)
    val transIndex = new SimpleFeatureIndex(TransitionFeats)

    val k = 10

    // model domains
    @domain case class Theta(A: Mat, w: Vect, wb: Vect)

    implicit val Thetas = Theta.Values(Matrices(k, numFeats), Vectors(k), TransitionFeats)
    implicit val maxProductParams = BPParameters(iterations = 2)

    val init = Settings(Thetas.createRandomSetting(random.nextGaussian() * 0.1))
    val params = AdaGradParameters(10, 0.1, 0.1, initParams = init)

    def local(x: X.Term, y: Y.Term, i: IntTerm) = {
      localIndex.oneHot('bias, y(i)) + localIndex.oneHot('word, y(i), x(i))
    }

    def transition(x: X.Term, y: Y.Term, i: IntTerm) = {
      transIndex.oneHot('pair, y(i), y(i + 1))
    }

    def model(w: Thetas.Term)(x: X.Term)(y: Y.Term) = {
      sum(0 until x.length) { i => w.w dot (w.A * local(x,y,i)) } +
      sum(0 until x.length - 1) { i => w.wb dot transition(x,y,i)}
    } subjectTo (y.length === x.length) argmaxBy maxProduct

    def hamming(yGold:Y.Term, y:Y.Term) =
      sum(0 until yGold.length) {i => 1.0 - I(yGold(i) === y(i))}

    def lossAugmented(t:Thetas.Term)(x:X.Term, y:Y.Term, yGold:Y.Term) =
      {model(t)(x)(y) + hamming(yGold,y)} argmaxBy maxProduct

    def learnObj(t:Thetas.Term) =
      sum(train.toConst){ xy => model(t)(xy._1)(xy._2) -
                                max(Y) {y => lossAugmented(t)(xy._1, y, xy._2)}}

    val thetaStar =
      argmax(Thetas)(theta => learnObj(theta)) by adaGrad(params)

    val predict = fun(X) { x => argmax(Y)(model(thetaStar.precalculate)(x)) }
  }


  object NeuralModel extends Model {

    implicit val index = new SimpleIndex()
    val transIndex = new SimpleIndex()

    import Data._

    val ak = 10
    val bk = 10

    // model domains
    @domain case class Theta(a: Mat, b: Mat, w: Vect, wb: Vect)

    implicit val Thetas = Theta.Values(Matrices(ak, bk), Matrices(bk, numFeats), Vectors(ak), TransitionFeats)
    implicit val maxProductParams = BPParameters(iterations = 2)

    def model(w: Thetas.Term)(x: X.Term)(y: Y.Term) = {
      sum(0 until x.length) { i => w.w dot (w.a * sigmVec(w.b * feature('bias, y(i)))) } +
        sum(0 until x.length) { i => w.w dot (w.a * sigmVec(w.b * feature('word, y(i) -> x(i)))) } +
        sum(0 until x.length - 1) { i => w.wb dot feature('pair, y(i) -> y(i + 1))(TransitionFeats, transIndex) }
    } subjectTo (y.length === x.length) argmaxBy Argmaxer.maxProduct

    val init = Settings(Thetas.createRandomSetting(random.nextGaussian() * 0.1))
    val params = AdaGradParameters(10, 0.1, 0.1, initParams = init)
    lazy val thetaStar =
      learn(Thetas)(t => perceptron(train.toConst)(Y)(model(t))) using Argmaxer.adaGrad(params)

    val predict = fun(X) { x => argmax(Y)(model(Thetas.Const(thetaStar))(x)) }
  }
  def run(m: Model, test: Seq[(IndexedSeq[String], IndexedSeq[String])]): Double = {
    run(m.predict, test)
  }

  def run(predict:IndexedSeq[String] => IndexedSeq[String], test: Seq[(IndexedSeq[String], IndexedSeq[String])]): Double = {
    var errs = 0.0
    var total = 0.0
    for ((x, y) <- test) {
      val yh = predict(x)
      assert(yh.length == y.length)
      for (i <- 0 until y.length) {
        if (yh(i) != y(i)) errs += 1.0
        total += 1.0
      }
    }
    errs / total
  }

//  val linErr = run(CRFModel, Data.test)
//  print(s"linErr: $linErr")
    val multErr = run(MultiTaskModel, Data.test)
    print(s"multErr: $multErr")
  //  val neuralErr = run(NeuralModel, Data.test)
  //  print(s"neuralErr: $neuralErr")

}
