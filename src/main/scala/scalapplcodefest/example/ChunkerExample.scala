package scalapplcodefest.example

import scalapplcodefest.Wolfe


/**
 *
 *
 * @author svivek
 */
object ChunkerExample {

  import Wolfe._

  case class Word(token: String)
  case class Tag(label: String)
  case class ChunkLabel(label: String)

  case class Sentence(words: Seq[Word], tags: Seq[Tag], chunks: Seq[ChunkLabel]) {
    val size = words.size
  }

  implicit val allData = all[Sentence]

  def model(sentence: Sentence, weights: Vector) = {
    import sentence._

    val f1 = sum(0 until size) {i => ft(chunks(i))}
    val f2 = sum(0 until size) {i => ft((words(i), chunks(i)))}
    val f3 = sum(0 until size) {i => ft((tags(i), chunks(i)))}
    val f4 = sum(0 until (size - 1)) {i => ft((chunks(i), chunks(i + 1)))}

    (f1 + f2 + f3 + f4) dot weights
  }

  def observation(sentence: Sentence) = (sentence.words, sentence.tags)

  @Operator.Argmax
  @Objective.LinearModel
  def predict(weights: Vector)(sentence: Sentence): Sentence =
    argmax {allData filter {s => observation(s) == observation(sentence)}} {s => model(s, weights)}

  @Operator.Max
  def predictionScore(sentence: Sentence, weights: Vector): Double =
    max {allData filter {s => observation(s) == observation(sentence)}} {s => model(s, weights)}

  def loss(data: Seq[Sentence])(weights: Vector): Double =
    sum(data) {sentence => predictionScore(sentence, weights) - model(sentence, weights)}

  @Objective.Adagrad(0.1)
  def learn(data: Seq[Sentence]): Vector =
    argmin(vectors)(loss(data))

  def predictor(data: Seq[Sentence]) = predict(learn(data)) _

  // The rest is boilerplate
  def makeExample(words: String, tags: String, chunks: String) = {
    val w = words.split(" ").toSeq.map(Word)
    val t = tags.split(" ").toSeq.map(Tag)
    val c = chunks.split(" ").toSeq.map(ChunkLabel)

    assert(w.length == t.length && w.length == c.length)

    Sentence(w, t, c)
  }

  val s1 = makeExample("Mary had a little lamb .", "NNP VBD DT JJ NN .", "B-NP B-VP B-NP I-NP I-NP O")
  val s2 = makeExample("She had the rest for lunch next day .",
    "PRP VBD DT NN IN NN JJ NN .",
    "B-NP B-VP B-NP I-NP B-PP B-NP B-NP I-NP")

  val data = Seq(s1, s2)

  val chunker = predictor(data)

  val testExample = makeExample("Mary had a little lunch .", "NNP VBD DT JJ NN .", "O O O O O O")

  val prediction = chunker(testExample)

  println(prediction)
}


