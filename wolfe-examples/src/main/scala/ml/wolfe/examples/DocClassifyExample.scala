package ml.wolfe.examples

import ml.wolfe.nlp.Document
import ml.wolfe.nlp.io.TwentyNewsGroupReader
import ml.wolfe.term.domain
import ml.wolfe.{FactorieVector, SimpleIndex}

/**
 * @author riedel
 */
object DocClassifyExample extends App {

  import ml.wolfe.term.TermImplicits._

  val (trainDocs, testDocs) = TwentyNewsGroupReader.loadFromTarGz()

  @domain case class Doc(feats: FactorieVector)
  @domain case class Instance(doc: Doc, label: String)

  def toInstance(doc: Document) = Instance(Doc(feats('words, doc.tokens.map(_.word))), doc.ir.docLabel.get)

  implicit val index = new SimpleIndex
  implicit val Labels = trainDocs.flatMap(_.ir.docLabel).distinct.toDom
  implicit val Docs = Doc.Values(Vectors(100000))
  implicit val Weights = Vectors(100000)
  implicit val Instances = Instance.Values(Docs, Labels)

  def model(w: Weights.Term, x: Docs.Term)(y: Labels.Term) = {
    w dot x.feats
  }

  val train = trainDocs.map(toInstance).toConst
  val test = testDocs.map(toInstance).toConst

  //def conjoin

}
