package ml.wolfe.examples

import cc.factorie.optimize.LBFGS
import ml.wolfe.nlp.io.CoNLLReader
import ml.wolfe.nlp.{SentenceSplitter, TokenSplitter}
import ml.wolfe.{Learn, D3Implicits, DualDecomposition, Wolfe}

/**
 * @author Sebastian Riedel
 */
object SkipChainLearn extends App {

  import ml.wolfe.Wolfe._
  import ml.wolfe.macros.OptimizedOperators._
  import SkipChainUtil._

  val corpus        = "/Users/sriedel/corpora/conll03/eng.train"
  val (test, train) = ml.wolfe.nlp.io.CoNLLReader.asDocs(corpus, " ").take(1000).toIndexedSeq.splitAt(10)
  val labels        = train.flatMap(_.entityMentionsAsBIOSeq).distinct

type Words = Seq[String]
type Ners = Seq[String]

def space(words: Words) = seqsOfLength(words.length, labels)
def feats(words: Words)(ner: Ners) =
  sum(0 until words.size) { i => oneHot(ner(i)) } +
  sum(0 until words.size) { i => oneHot(words(i) -> ner(i)) } +
  sum(0 until words.size) { i => oneHot('lowercase -> ner(i), I(words(i).head.isLower)) } +
  sum(0 until words.size) { i => oneHot('firstName -> ner(i), I(firstNames(words(i)))) } +
  sum(0 until words.size) { i => oneHot('lastName -> ner(i), I(lastNames(words(i)))) } +
  sum(0 until words.size) { i => oneHot('location -> ner(i), I(locations(words(i)))) } +
  sum(0 until words.size) { i => oneHot('punct -> ner(i), I(puncts(words(i)))) } +
  sum(0 until words.size - 1) { i => oneHot(ner(i) -> ner(i + 1)) }

  def crf(w: Vector, words: Words)(ners: Ners) =
    w dot feats(words)(ners)

  def lossAugmented(weights: Vector, words: Words, gold: Ners)(guess: Ners) =
    crf(weights, words)(guess) + sum(0 until words.size) { i => -1.0 * I(gold(i) == guess(i)) }

  def instanceNegLoss(weights: Vector, words: Words)(gold: Ners) =
  //    crf(weights, words)(gold) - max(space(words))(crf(weights, words))
    crf(weights, words)(gold) - max(space(words))(lossAugmented(weights, words, gold))

  //@OptimizeByLearning(Learn.batch(new LBFGS(10)))
  def negLoss(data: Seq[(Words, Ners)])(weights: Vector) =
    sum(data)(d => instanceNegLoss(weights, d._1)(d._2))

  val trainPairs  = train.map(d => (d.tokenWords, d.entityMentionsAsBIOSeq))
  val testPairs   = test.map(d => (d.tokenWords, d.entityMentionsAsBIOSeq))
  val trainInputs = train.map(_.tokenWords)
  val testInputs  = test.map(_.tokenWords)


  val weights = argmax(vectors) { negLoss(trainPairs) }

  def predict(words: Words) = argmax(space(words)) { crf(weights, words) }

  val trainPredictions = map(trainInputs) { predict }
  val testPredictions  = map(testInputs) { predict }


  println(accuracy(trainPairs.map(_._2), trainPredictions))
  println(accuracy(testPairs.map(_._2), testPredictions))


  println(appendMentions(train(1), trainPredictions(1)))
  println(appendMentions(test(1), testPredictions(1)))

}
