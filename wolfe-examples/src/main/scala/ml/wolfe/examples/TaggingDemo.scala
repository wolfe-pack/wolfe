package ml.wolfe.examples

import cc.factorie.la.DenseTensor1
import ml.wolfe.SimpleIndex
import ml.wolfe.nlp._
import ml.wolfe.term.TermImplicits._
import ml.wolfe.term._
import ml.wolfe.term.Argmaxer._
import ml.wolfe.term.LearningObjective._

/**
 * Created by luke on 29/05/15.
 */
object TaggingDemo extends App {
  val doc = TokenSplitter(SentenceSplitter(
    "John Denver is a Songwriter. Denver has produced many records"))
  val words = doc.tokens.map(_.word).distinct
  val tags = Seq("O", "B-LOC", "I-LOC", "B-PER", "I-PER")

  val maxLength = 15
  implicit val Words = words.toDom withOOV "[OOV]"
  implicit val Tags = tags.toDom
  implicit val Y = Seqs(Tags, 0, maxLength)
  implicit val Weights = Vectors(dim = 1000)


  implicit val index = new SimpleIndex()
  implicit val maxProductParams = BPParameters(iterations = 2)

  val firstName = Set("John", "Jack")
  val lastName = Set("Denver")
  val location = Set("Denver", "Dallas")
  val punct     = Set(",", ".", "?", ";")
  def lowercase(w: Words.Value) = w.head.isLower

  def model(w: Weights.Term)(x: Seq[Words.Value])(y: Y.Term) = {
    def matches = matchingPairs(x)
    sum(0 until x.length) { i => w dot feature('bias, y(i)) } +
    sum(0 until x.length) { i => w dot feature('word, y(i) -> Words.Const(x(i))) } +
    sum(0 until x.length) { i => w dot feature('firstName, I(Bools.Const(firstName(x(i)))), y(i)) } +
    sum(0 until x.length) { i => w dot feature('lastName, I(Bools.Const(lastName(x(i)))), y(i)) } +
    sum(0 until x.length) { i => w dot feature('location, I(Bools.Const(location(x(i)))), y(i)) } +
    sum(0 until x.length) { i => w dot feature('lowercase, I(Bools.Const(lowercase(x(i)))), y(i)) } +
    sum(0 until x.length) { i => w dot feature('punct, I(Bools.Const(punct(x(i)))), y(i)) } +
    sum(0 until x.length - 1) { i => w dot feature('pair, y(i) -> y(i + 1)) } +
    sum(0 until matches.length) { i => w dot feature('match, y(matches(i)._1) -> y(matches(i)._2))}
  } subjectTo (y.length === x.length)

//  val wStar = new DenseTensor1(Seq(
//    feature('location,  1.1, Tags.Const("B-LOC")),
//    feature('lastName,  1.0, Tags.Const("B-PER")),
//    feature('firstName, 3.0, Tags.Const("B-PER")),
//    feature('lowercase, 1.0, Tags.Const("O")),
//    feature('punct, 1.0, Tags.Const("O")),
//    feature('pair, 2.0, Tags.Const("B-PER") -> Tags.Const("I-PER")),
//    feature('bias, 1.0, Tags.Const("O")),
//    feature('match, 2.0, Tags.Const("B-PER") -> Tags.Const("B-PER")),
//    feature('match, 2.0, Tags.Const("B-PER") -> Tags.Const("I-PER")),
//    feature('match, 2.0, Tags.Const("I-PER") -> Tags.Const("I-PER")),
//    feature('match, 2.0, Tags.Const("I-PER") -> Tags.Const("B-PER"))
//  ).map(_.eval()).reduce(_+_))

  val wStar = new DenseTensor1(Seq(
      feature('match, 1.1, Tags.Const("I-PER") -> Tags.Const("B-PER")),
      feature('location,  1.1, Tags.Const("B-LOC")),
      feature('lastName,  0.7, Tags.Const("B-PER")),
      feature('lastName,  0.3, Tags.Const("I-PER")),
      feature('firstName, 2, Tags.Const("B-PER")),
      feature('pair, 0.9, Tags.Const("B-PER") -> Tags.Const("I-PER")),
      feature('bias, 1, Tags.Const("O"))
    ).map(_.eval()).reduce(_+_))

  def matchingPairs(x: Seq[Words.Value]) =
    for (j <- 0 until x.length; i <- 0 until j; if x(i) != "." && x(i) == x(j)) yield (i, j)

  def predict(x: Seq[Words.Value]) = argmax(Y) {
    model(Weights.Const(wStar))(x)
  } by maxProduct

  val xTest = doc.tokens.map(_.word)
  val yStar = predict(xTest).eval()
//val yStar = IndexedSeq("B-PER", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O")

  println(xTest)
  println(yStar)
  println(model(Weights.Const(wStar))(xTest)(Y.Const(yStar)).eval())
  //println(best.eval())



}
