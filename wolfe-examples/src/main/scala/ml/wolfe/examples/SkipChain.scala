package ml.wolfe.examples

import ml.wolfe.nlp.{SentenceSplitter, TokenSplitter}
import ml.wolfe.{DualDecomposition, Wolfe}

/**
 * @author Sebastian Riedel
 */
object SkipChain extends App{

  import ml.wolfe.Wolfe._
  import ml.wolfe.macros.OptimizedOperators._

  val labels = Seq("O","B-PER","B-LOC","I-PER","I-LOC")
  val cities = Set("Denver")
  val weights = Vector(
    ("Denver","B-LOC") -> 1.0,
    ("Denver","B-PER") -> 0.1,
    ("John","B-PER") -> 3.0,
    ("B-PER","I-PER") -> 2.0,
    ("B-PER","O") -> 0.0,
    ("O","I-PER") -> -2.0,
    ('lowercase,"O") -> 2.0,
    (",","O") -> 1.0,
    "O" -> 1.0
  )
  val doc = TokenSplitter(SentenceSplitter(
    "John Denver is a Songwriter. Throughout his life, Denver produced many records."))
  val words = doc.tokenWords
  val ners = doc.entityMentionsAsBIOSeq
  type Words = Seq[String]
  type Ners = Seq[String]
  def space(words:Words) = seqsOfLength(words.length,labels)
  def feats(words:Words)(ner:Ners) =
    sum(0 until words.size) { i => oneHot(ner(i))} +
    sum(0 until words.size) { i => oneHot('lowercase -> ner(i), I(words(i).head.isLower))} +
    sum(0 until words.size) { i => oneHot(words(i) -> ner(i))} +
    sum(0 until words.size - 1) {i => oneHot(ner(i) -> ner(i+1))}
  def crf(w:Vector,words:Words)(ners:Ners) =
    w dot feats(words)(ners)

  def matches(words:Words) =
  for (i <- words.indices; j <- words.indices; if words(i) != "." && i < j && words(i) == words(j)) yield (i,j)

  def skip(words:Words)(ners:Ners) =
    sum(matches(words)) {p => 2.0 * I(ners(p._1).drop(2) == ners(p._2).drop(2))}

//  @OptimizeByInference(BeliefPropagation.maxProduct(10))
  @OptimizeByInference(DualDecomposition(_,10))
  def skipChain(w:Vector, words:Words)(ners:Ners) =
    crf(w,words)(ners) + skip(words)(ners)

  val prediction = argmax(space(words)){skipChain(weights,words)}
  println(prediction)
  println(words zip prediction)


  println(weights dot feats(Seq("produced"))(Seq("O")))
  println(feats(words)(prediction))
  val fixed = Seq("B-PER", "I-PER", "O", "O", "O", "O", "O", "O", "O", "O", "B-PER", "O", "O", "O", "O")
  println(skipChain(weights,words)(prediction))
  println(skipChain(weights,words)(fixed))
  println(fixed)

}
