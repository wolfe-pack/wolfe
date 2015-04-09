package ml.wolfe.examples

import ml.wolfe._
import ml.wolfe.nlp.Document
import ml.wolfe.nlp.io.CoNLLReader
import ml.wolfe.term.Argmaxer._
import ml.wolfe.term.LearningObjective._
import ml.wolfe.term.TermImplicits._
import ml.wolfe.term._

/**
 * @author riedel
 * @author rockt
 */
object NERDemo extends App {
  implicit val index = new SimpleIndex()
  implicit val indexer = new DefaultIndexer(index)

  implicit def tokensToInput(tokens: Seq[String]): Input = {
    val biasFeatures = tokens.toIndexedSeq.map(t => feats("Blah"))
    val transitionFeatures = tokens.toIndexedSeq.map(t => feats("Blubs"))
    Input(biasFeatures, transitionFeatures)
  }

  //data
  val train: Seq[(Input, Output)] =
    Seq(tokensToInput(Seq("My", "name", "is", "Wolfe", "!")) -> IndexedSeq("O", "O", "O", "B-PER", "O"))
  val test: Seq[(Input, Output)] =
    Seq(tokensToInput(Seq("Wolfe", "is", "awesome", ".")) -> IndexedSeq("B-PER", "O", "O", "O"))

  val sentences = train ++ test
  val labels = sentences.flatMap(_._2).distinct

  //model definition
  val maxLength = sentences.map(_._2.length).max
  val maxFeats = 100
  implicit val Thetas = Vectors(maxFeats)
  implicit val Labels = labels.toDom

  @domain case class Input(biasFeatures:IndexedSeq[Vect], transitionFeatures:IndexedSeq[Vect])
  implicit val Inputs = Input.Values(Seqs(Vectors(maxFeats),0,maxLength), Seqs(Vectors(maxFeats),0,maxLength))
  type Output = IndexedSeq[String]
  implicit val Outputs = Seqs(Labels, 0, maxLength)
  implicit val Instances = Pairs(Inputs, Outputs)

  def model(t: Thetas.Term)(x: Inputs.Term)(y: Outputs.Term) =
    sum(0 until x.biasFeatures.length)(i => t dot (x.biasFeatures(i) conjoin feature(y(i)))) +
    sum(0 until x.biasFeatures.length - 1)(i => t dot (x.transitionFeatures(i) conjoin feature(y(i) -> y(i+1))))

  lazy val predict = fun(Inputs) { x => argmax(Outputs)(model(Thetas.Const(thetaStar))(x)) by maxProduct(MaxProductParameters(iterations = 10)) }
  val params = AdaGradParameters(epochs = 100, learningRate = 0.1)
  lazy val thetaStar = learn(Thetas)(t => perceptron(train.toConst)(Outputs)(model(t))) using Argmaxer.adaGrad(params)

  def classify(input:Input):Output = predict(input)

  test.foreach(p => println(classify(p._1)))
}

object NERDemoHelper extends App {
  def toDocuments(sentences: Seq[String]): Seq[Document] = {
    val reader = new CoNLLReader(filename = null)
    sentences.map(s => {
      val sentence = reader.fromCoNLL2003(s.split("\n"))
      Document(sentence.toText, IndexedSeq(sentence))
    })
  }
}
