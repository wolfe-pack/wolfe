package ml.wolfe.examples

import ml.wolfe._
import ml.wolfe.nlp.{Sentence, Document}
import ml.wolfe.nlp.io.CoNLLReader
import ml.wolfe.term.Argmaxer._
import ml.wolfe.term.LearningObjective._
import ml.wolfe.term.TermImplicits._
import ml.wolfe.term._
import ml.wolfe.nlp.{Sentence, Document}

object ChunkingDemo extends App {
  implicit val index = new SimpleIndex()
  @domain case class Input(words: IndexedSeq[String])
  type Output = IndexedSeq[String]
  type Instance = (Input, Output)

//  val connl2000Train = new CoNLLReader("wolfe/wolfe-examples/src/main/resources/ml/wolfe/datasets/conll2000/train.txt", " ")
//  val connl2000Test = new CoNLLReader("wolfe/wolfe-examples/src/main/resources/ml/wolfe/datasets/conll2000/test.txt", " ")
//  def getChunkTags(s:Sentence) = {
//    val chunkTags = Array.fill(s.tokens.length)("O")
//    s.ie.entityMentions.foreach { chunk =>
//      chunkTags(chunk.start) = if (chunk.label == "O") "O" else "B-" + chunk.label
//      for (i <- chunk.start+1 until chunk.end) chunkTags(i) = "I-" + chunk.label
//    }
//    collection.immutable.IndexedSeq(chunkTags:_*)
//  }
//  def toInstance(s:Sentence):(Input, Output) = {
//    Input(s.tokens.map(_.word.toString)) -> getChunkTags(s)
//  }
//  val train = connl2000Train.take(50).map(toInstance).toIndexedSeq
//  val test = connl2000Test.take(10).map(toInstance).toIndexedSeq
  val train = Seq(
    Input(IndexedSeq("Welcome", "to", "Wolfe", "!")) -> IndexedSeq("O", "O", "WOLFE", "omg"),
    Input(IndexedSeq("Wolfe", "is", "great", "!")) -> IndexedSeq("WOLFE", "O", "O", "omg")
  )
  val test = Seq(
    Input(IndexedSeq("Wolfe", "is", "awesome", "!")) -> IndexedSeq("WOLFE", "O", "O", "omg"),
    Input(IndexedSeq("I", "love", "Wolfe", ".")) -> IndexedSeq("O", "O", "WOLFE", "omg")
  )


  val sentences = train ++ test
  val labels = sentences.flatMap(_._2).distinct
  val words = (train flatMap (_._1.words)).distinct

  //model definition
  val maxLength = sentences.map(_._2.length).max
  val maxFeats = 20000

  implicit val Thetas = Vectors(maxFeats)
  implicit val Labels = labels.toDom
  implicit val Words = words.toDom withOOV "[OOV]"

  implicit val Inputs = Input.Objects(Seqs(Words, 0, maxLength))
  implicit val Outputs = Seqs(Labels, 0, maxLength)
  implicit val Instances = Pairs(Inputs, Outputs)

  def model(t: Thetas.Term)(x: Inputs.Term)(y: Outputs.Term) = {
    sum(0 until x.words.length)(i => t dot feature('word, x.words(i), y(i)))
  } subjectTo (y.length === x.words.length) argmaxBy maxProduct(mpParams)

  val params = AdaGradParameters(epochs = 5, learningRate = 0.1)
  val mpParams = BPParameters(iterations = 3)
  val thetaStar = learn(Thetas)(t => perceptron(train.toConst)(Outputs)(model(t))) using adaGrad(params)
  println("Theta* = ")
  println(thetaStar.toIndexedString)

  val predict = fun(Inputs) { x => argmax(Outputs)(model(Thetas.Const(thetaStar))(x)) }
  println("Prediction = ")
  test.foreach(p => {
    val words = p._1.words
    val labels = predict(p._1)
    println((words zip labels).map(x => x._1 + "/" + x._2))
  })

  // chunking demo
  val n = 5
  val Y = Seqs(Bools, 0, n)
  def model(length: IntTerm)(y: Y.Term) = {
    sum(0 until length) { i => I(y(i))} +
      sum(0 until length - 1) { i => I(y(i) <-> ! y(i + 1))}
  }
  val result = argmax(Y)(y => model(5)(y) subjectTo (y.length === 5) argmaxBy maxProduct(mpParams)).evalResult()
  result.factorGraphs.head
}