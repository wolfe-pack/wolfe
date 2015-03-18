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

  val (trainDocs, testDocs) = TwentyNewsGroupReader.loadFromTarGz()

  @domain case class Doc(feats: FactorieVector)

  def toInstance(doc: Document) = (Doc(feats('words, doc.tokens.take(10).map(_.word))), doc.ir.docLabel.get)

  implicit val random = new Random(0)
  implicit val index = new SimpleIndex
  implicit val Labels = trainDocs.flatMap(_.ir.docLabel).distinct.toDom
  implicit val Docs = Doc.Values(Vectors(100000))
  implicit val Weights = Vectors(100000)
  implicit val Instances = Docs x Labels
  implicit val adaParam = AdaGradParameters(10, 0.1)

  def model(w: Weights.Term)(x: Docs.Term)(y: Labels.Term) = {
    w dot (x.feats conjoin feature(y))
  }

  def learnObj(data: Instances.SeqTerm)(w: Weights.Term) = {
    perceptron(data)(Labels)(model(w))
  }

  val trainInstances = random.shuffle(trainDocs).take(10).map(toInstance)
  val testInstances = testDocs.take(10).map(toInstance)
  val train = trainInstances.toConst
  val test = testInstances.toConst

  println(index.size)
  println(Labels.mkString("\n"))
  println(trainInstances.take(10).map(_._2).mkString("\n"))

  val wStar = (argmax(Weights)(learnObj(train)) by adaGrad).eval()

  val predict = lambda(Docs) { d => argmax(Labels)(model(wStar.toConst)(d))}

  println(predict(trainInstances(0)._1))

  //println(wStar)

  //def conjoin

}
