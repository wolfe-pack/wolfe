package ml.wolfe.examples

import ml.wolfe.nlp.io.CoNLLReader
import ml.wolfe.nlp.{Sentence, Document, SentenceSplitter, TokenSplitter}
import ml.wolfe.{D3Implicits, DualDecomposition, Wolfe}

///**
// * @author Sebastian Riedel
// */
//object SkipChain extends App {
//
//  import ml.wolfe.Wolfe._
//  import ml.wolfe.macros.OptimizedOperators._
//  import SkipChainUtil._
//
//  val doc   = TokenSplitter(SentenceSplitter(
//    "John Denver is a Songwriter. Throughout his life, Denver produced many records in Dallas."))
//  val words = doc.tokenWords
//  val ners  = doc.entityMentionsAsBIOSeq
//
//  type Words = Seq[String]
//  type Ners = Seq[String]
//  def space(words: Words) = seqsOfLength(words.length, labels)
//  def feats(words: Words)(ner: Ners) =
//    sum(0 until words.size) { i => oneHot(ner(i)) } +
//    sum(0 until words.size) { i => oneHot(words(i) -> ner(i)) } +
//    sum(0 until words.size) { i => oneHot('lowercase -> ner(i), I(words(i).head.isLower)) } +
//    sum(0 until words.size) { i => oneHot('firstName -> ner(i), I(firstNames(words(i)))) } +
//    sum(0 until words.size) { i => oneHot('lastName -> ner(i), I(lastNames(words(i)))) } +
//    sum(0 until words.size) { i => oneHot('location -> ner(i), I(locations(words(i)))) } +
//    sum(0 until words.size) { i => oneHot('punct -> ner(i), I(puncts(words(i)))) } +
//    sum(0 until words.size - 1) { i => oneHot(ner(i) -> ner(i + 1)) }
//
//  def crf(w: Vector, words: Words)(ners: Ners) =
//    w dot feats(words)(ners)
//
//  def matches(words: Words) =
//    for (i <- words.indices; j <- words.indices; if words(i) != "." && i < j && words(i) == words(j)) yield (i, j)
//
//  def skip(words: Words)(ners: Ners) =
//    sum(matches(words)) { p => 2.0 * I(ners(p._1).drop(2) == ners(p._2).drop(2)) }
//
//  //  @OptimizeByInference(BeliefPropagation.maxProduct(10))
//  @OptimizeByInference(DualDecomposition(_, 10))
//  def skipChain(w: Vector, words: Words)(ners: Ners) =
//    crf(w, words)(ners) + skip(words)(ners)
//
//  val old = argmax(space(words)) { crf(weights, words) }
//
//  println(old)
//
//  val prediction = argmax(space(words)) { skipChain(weights, words) }
//  println(prediction)
//  println(words zip prediction)
//
//
//  println(weights dot feats(Seq("produced"))(Seq("O")))
//  println(feats(words)(prediction))
//
//  D3Implicits.saveD3Graph(FactorGraphBuffer.get(), "/tmp/fg.html")
//
//  //println(D3Implicits.factorGraphURL(FactorGraphBuffer).source)
//
//  val result = appendMentions(doc, prediction)
//  println(result)
//
//}

//object SkipChainUtil {
//
//  import Wolfe._
//
//  val labels     = Seq("O", "B-PER", "B-LOC", "I-PER", "I-LOC")
//  val locations  = Set("Denver", "Dallas")
//  val firstNames = Set("John")
//  val lastNames  = Set("Denver")
//  val puncts     = Set(",", ".", "?", ";")
//  val weights    = Vector(
//    ('location, "B-LOC") -> 1.1,
//    ('lastName, "B-PER") -> 1.0,
//    ('firstName, "B-PER") -> 3.0,
//    ('lowercase, "O") -> 2.0,
//    ('punct, "O") -> 1.0,
//    ("B-PER", "I-PER") -> 2.0,
//    "O" -> 1.0)
//
//  def appendMentions(doc: Document, tags: Seq[String]) = {
//    def append(sentences: List[Sentence], tags: Seq[String], result: List[Sentence] = Nil): List[Sentence] = {
//      sentences match {
//        case Nil => result
//        case h :: t =>
//          val mentions = CoNLLReader.collectMentions(tags.take(h.tokens.size))
//          val appended = h.copy(ie = h.ie.copy(entityMentions = h.ie.entityMentions ++ mentions))
//          append(t, tags.drop(h.tokens.size), appended :: result)
//      }
//    }
//    doc.copy(sentences = append(doc.sentences.toList, tags).reverse.toIndexedSeq)
//  }
//
//
//  def accuracy(gold: Seq[Seq[String]], guess: Seq[Seq[String]]) =
//    (gold.iterator.flatten zip guess.iterator.flatten).map { case (y, yp) => I(y == yp) }.sum / gold.map(_.size).sum
//
//
//  def main(args: Array[String]) {
//    import ml.wolfe.Wolfe._
//    import ml.wolfe.macros.OptimizedOperators._
//
//    def space(words: Seq[String]) =
//      seqsOfLength(words.length, Seq("O", "LOC", "CONF"))
//    def feats(words: Seq[String])(ner: Seq[String]) =
//      sum(0 until words.size) { i => oneHot(words(i) -> ner(i)) } +
//      sum(0 until words.size - 1) { i => oneHot(ner(i) -> ner(i + 1)) }
//    def crf(weights: Vector, words: Seq[String])(ner: Seq[String]) =
//      weights dot feats(words)(ner)
//    def svmNegLoss(words: Seq[String], gold: Seq[String])(weights: Vector) =
//      crf(weights, words)(gold) - max(space(words)) { y =>
//        crf(weights, words)(y) +
//        sum(0 until words.size) { i => -1.0 * I(gold(i) == y(i)) }
//      }
//    val x = Seq("NIPS", "at", "Montreal")
//    argmax(vectors) { svmNegLoss(x, Seq("CONF", "O", "LOC")) }
//  }
//
//}


