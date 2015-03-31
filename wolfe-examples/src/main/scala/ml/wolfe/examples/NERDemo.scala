package ml.wolfe.examples

import cc.factorie.la.DenseTensor1
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
  //data input
  import NERDemoHelper._
  val sentences = Seq(
    """U.N. NNP B-NP B-ORG
      |official NN I-NP O
      |Ekeus NNP I-NP B-PER
      |heads VBZ B-VP O
      |for IN B-PP O
      |Baghdad NNP B-NP B-LOC
      |. . O O""".stripMargin,
    """EU NNP B-NP B-ORG
      |rejects VBZ B-VP O
      |German JJ B-NP B-MISC
      |call NN I-NP O
      |. . O O """.stripMargin)

  val documents = toDocuments(sentences)
  val train = documents.take(1)
  val test = documents.tail

  val labels = documents.flatMap(_.entityMentionsAsBIOSeq).distinct
  val trainPairs = documents.map(d => (d.tokenWords, d.entityMentionsAsBIOSeq))
  val testPairs = test.map(d => (d.tokenWords, d.entityMentionsAsBIOSeq))
  val trainInputs = train.map(_.tokenWords)
  val testInputs = test.map(_.tokenWords)

  //model definition
  val maxFeats = 100
  val maxLength = documents.flatMap(_.sentences.map(_.tokens.length)).max
  implicit val index = new SimpleIndex
  implicit val Thetas = Vectors(maxFeats)
  implicit val Labels = labels.toDom

  @domain case class Input(biasFeatures:IndexedSeq[Vect], transitionFeatures:IndexedSeq[Vect])
  implicit val Inputs = Input.Values(Seqs(Vectors(maxFeats),0,maxLength), Seqs(Vectors(maxFeats),0,maxLength))
  type Output = IndexedSeq[String]
  implicit val Outputs = Seqs(Labels, 0, maxLength)
  implicit val Instances = Pairs(Inputs, Outputs)

  def model(t: Thetas.Term)(x: Inputs.Term)(y: Outputs.Term) = {
    val local = sum(0 until x.biasFeatures.length)(i => t dot (x.biasFeatures(i) conjoin feature(y(i))))
    val pairwise = sum(0 until x.biasFeatures.length - 1)(i => t dot (x.transitionFeatures(i) conjoin feature(y(i) -> y(i+1))))
    (local + pairwise) argmaxBy maxProduct(MaxProductParameters(iterations = 10))
  }

  lazy val predict = fun(Inputs) { x => argmax(Outputs)(model(Thetas.Const(thetaStar))(x))}
  val params = AdaGradParameters(epochs = 100, learningRate = 0.1)
  lazy val thetaStar = learn(Thetas)(t => perceptron(data.toConst)(Outputs)(model(t))) using Argmaxer.adaGrad(params)

  def classify(input:Input):Output = predict(input)

  implicit def tokensToInput(tokens: Seq[String]): Input = {
    val biasFeatures = tokens.toIndexedSeq.map(t => feats("Blah"))
    val transitionFeatures = tokens.toIndexedSeq.map(t => feats("Blubs"))
    Input(biasFeatures, transitionFeatures)
  }

  def data: Seq[(Input,Output)] = trainPairs.map(p => (tokensToInput(p._1), p._2))

  //training
  println(train.head)
  println(test.head)
  thetaStar //does the training
  println(classify(Seq("My", "name", "is", "Wolfe", "!")))
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
