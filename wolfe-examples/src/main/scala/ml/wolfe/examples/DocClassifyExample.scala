package ml.wolfe.examples

import ml.wolfe.nlp.Document
import ml.wolfe.nlp.io.TwentyNewsGroupReader
import ml.wolfe.term._
import ml.wolfe.{FactorieVector, SimpleIndex}
import ml.wolfe.term.Argmaxer._

import scala.util.Random

/**
 * @author riedel
 */
object DocClassifyExample extends App {

  import ml.wolfe.term.TermImplicits._
  import LearningObjective._

  implicit val random = new Random(0)
  implicit val index = new SimpleIndex

  val (trainDocs, testDocs) = TwentyNewsGroupReader.loadFromTarGz()

  @domain case class Doc(feats: FactorieVector)

  def toInstance(doc: Document) = (Doc(feats('words, doc.tokens.take(10).map(_.word))), doc.ir.docLabel.get)

  implicit val Labels = trainDocs.flatMap(_.ir.docLabel).distinct.toDom
  implicit val Docs = Doc.Values(Vectors(100000))
  implicit val Weights = Vectors(100000)
  implicit val Instances = Docs x Labels

  implicit val adaParam = AdaGradParameters(10, 0.1)

  val trainInstances = random.shuffle(trainDocs).take(10).map(toInstance)
  val testInstances = testDocs.take(10).map(toInstance)
  val train = trainInstances.toConst
  val test = testInstances.toConst

  println(index.size)
  println(Labels.mkString("\n"))
  println(trainInstances.take(10).map(_._2).mkString("\n"))

  def model(w: Weights.Term)(x: Docs.Term)(y: Labels.Term) = {
    w dot (x.feats conjoin feature(y))
  }

  val wStar = learn(Weights)(w => perceptron(train)(Labels)(model(w))) using adaGrad
  val predict = fun(Docs) { d => argmax(Labels)(model(wStar.toConst)(d))}

  println(predict(trainInstances(0)._1))


}

object DocClassifyExampleNew extends App {

  import ml.wolfe.term.TermImplicits._
  import LearningObjective._

  implicit val random = new Random(0)
  implicit val index = new SimpleIndex

  val (trainDocs, testDocs) = TwentyNewsGroupReader.loadFromTarGz()

  @domain case class Doc(feats: FactorieVector)
  @domain case class Theta(weights:Map[String,FactorieVector])

  def toInstance(doc: Document) = (Doc(feats('words, doc.tokens.take(10).map(_.word))), doc.ir.docLabel.get)

  implicit val Feats = Vectors(10000)
  implicit val Labels = trainDocs.flatMap(_.ir.docLabel).distinct.toDom
  implicit val Thetas = Theta.Values(Labels -> Feats)
  implicit val Docs = Doc.Values(Feats)
  implicit val Instances = Docs x Labels

  implicit val adaParam = AdaGradParameters(10, 0.1)

  val trainInstances = random.shuffle(trainDocs).take(10).map(toInstance)
  val testInstances = testDocs.take(10).map(toInstance)
  val train = trainInstances.toConst
  val test = testInstances.toConst

  println(index.size)
  println(Labels.mkString("\n"))
  println(trainInstances.take(10).map(_._2).mkString("\n"))

  def model(t: Thetas.Term)(x: Docs.Term)(y: Labels.Term) = {
    t.weights(y) dot x.feats
  }

  val wStar = learn(Thetas)(t => perceptron(train)(Labels)(model(t))) using adaGrad
  val predict = fun(Docs) { d => argmax(Labels)(model(wStar.toConst)(d))}

  println(predict(trainInstances(0)._1))


}
