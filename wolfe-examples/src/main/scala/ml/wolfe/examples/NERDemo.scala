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

  val params = AdaGradParameters(epochs = 100, learningRate = 0.1)
  val mpParams = MaxProductParameters(iterations = 10)

  implicit val index = new SimpleIndex()

  @domain case class Input(word: IndexedSeq[String])

  type Output = IndexedSeq[String]
  type Instance = (Input, Output)

  def local(x: Inputs.Term, y: Outputs.Term, i: IntTerm) = cached(x, y(i)) {
    feature('bias, y(i)) +
      feature('word, x.word(i), y(i))
  }

  def pairwise(x: Inputs.Term, y: Outputs.Term, i: IntTerm) = cached(x, y(i)) {
    feature('trans, y(i), y(i + 1))
  }

  //data
  val train =
    Seq(Input(IndexedSeq("My", "name", "is", "Wolfe", "!")) -> IndexedSeq("O", "O", "O", "B-PER", "O"))
  val test =
    Seq(Input(IndexedSeq("Wolfe", "is", "awesome", ".")) -> IndexedSeq("B-PER", "O", "O", "O"))

  val sentences = train ++ test
  val labels = sentences.flatMap(_._2).distinct
  val words = (train flatMap (_._1.word)).distinct


  //model definition
  val maxLength = sentences.map(_._2.length).max
  val maxFeats = 100

  implicit val Thetas = Vectors(maxFeats)
  implicit val Labels = labels.toDom
  implicit val Words = words.toDom

  implicit val Inputs = Input.Objects(Seqs(Words, 0, maxLength))
  implicit val Outputs = Seqs(Labels, 0, maxLength)
  implicit val Instances = Pairs(Inputs, Outputs)

  def model(t: Thetas.Term)(x: Inputs.Term)(y: Outputs.Term) = {
    sum(0 until x.word.length)(i => t dot local(x, y, i)) +
      sum(0 until x.word.length - 1)(i => t dot pairwise(x, y, i))
  } argmaxBy maxProduct(mpParams)


  val thetaStar = learn(Thetas)(t => perceptron(train.toConst)(Outputs)(model(t))) using adaGrad(params)

  val predict = fun(Inputs) { x => argmax(Outputs)(model(Thetas.Const(thetaStar))(x)) }

  test.foreach(p => println(predict(p._1)))

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
