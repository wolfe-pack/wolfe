package ml.wolfe.examples

import ml.wolfe.nlp.Document
import ml.wolfe.nlp.io.TwentyNewsGroupReader
import ml.wolfe.term.{AdaGradParameters, SeqTerm, VarSeqTerm, domain}
import ml.wolfe.{FactorieVector, SimpleIndex}
import ml.wolfe.term.Argmaxer._

import scala.util.Random

/**
 * @author riedel
 */
object DocClassifyExample extends App {

  import ml.wolfe.term.TermImplicits._

  val (trainDocs, testDocs) = TwentyNewsGroupReader.loadFromTarGz()

  @domain case class Doc(feats: FactorieVector)

  @domain case class Instance(doc: Doc, label: String)

  def toInstance(doc: Document) = Instance(Doc(feats('words, doc.tokens.take(10).map(_.word))), doc.ir.docLabel.get)

  implicit val random = new Random(0)
  implicit val index = new SimpleIndex
  implicit val Labels = trainDocs.flatMap(_.ir.docLabel).distinct.toDom
  implicit val Docs = Doc.Values(Vectors(100000))
  implicit val Weights = Vectors(100000)
  implicit val Instances = Instance.Values(Docs, Labels)
  implicit val adaParam = AdaGradParameters(10,0.1)

  def model(w: Weights.Term, x: Docs.Term)(y: Labels.Term) = {
    w dot (x.feats conjoin feature(y))
  }

  def learnObj(data: Instances.SeqTerm)(w: Weights.Term) = {
    shuffled(data) { i => max(Labels) {l => model(w,i.doc)(l)} - model(w,i.doc)(i.label)} argmaxBy adaGrad
  }

  val trainInstances = trainDocs.take(10).map(toInstance)
  val testInstances = testDocs.take(10).map(toInstance)
  val train = trainInstances.toConst
  val test = testInstances.toConst

  println(index.size)

  val wStar = argmax(Weights)(learnObj(train)).eval()

  val predict = argmax(Labels)(model(wStar.toConst,Docs.Var))

  println(predict.eval(trainInstances(0).doc))

  //println(wStar)

  //def conjoin

}
