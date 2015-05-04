package ml.wolfe.examples

import ml.wolfe._
import ml.wolfe.model.LinearChain
import ml.wolfe.nlp.{Token, Sentence, Document}
import ml.wolfe.nlp.io.CoNLLReader
import ml.wolfe.term.Argmaxer._
import ml.wolfe.term.LearningObjective._
import ml.wolfe.term.TermImplicits._
import ml.wolfe.term._

/**
 * @author riedel
 * @author rockt
 */

object ChunkingDemo extends App {

  val train = new CoNLLReader("wolfe-examples/src/main/resources/ml/wolfe/datasets/conll2000/train.txt", " ").take(2).toIndexedSeq
  //val test = new CoNLLReader("wolfe-examples/src/main/resources/ml/wolfe/datasets/conll2000/test.txt", " ").take(2).toIndexedSeq


  def getChunkTags(s:Sentence) = {
    val chunkTags = Array.fill(s.tokens.length)("O")
    s.ie.entityMentions.foreach { chunk =>
      chunkTags(chunk.start) = if (chunk.label == "O") "O" else "B-" + chunk.label
      for (i <- chunk.start+1 until chunk.end) chunkTags(i) = "I-" + chunk.label
    }
    collection.immutable.IndexedSeq(chunkTags:_*)
  }

  val labels = train.flatMap(getChunkTags).distinct
  implicit val index = new SimpleIndex
  val input = for(s <- train) yield LinearChain.Input(
    s.tokens.map(t => feats(t.posTag) + feats(t.word)),
    s.tokens.map(t => feats(t.posTag) + feats(t.word))
  )
  val output = train.map(getChunkTags)
  val params = AdaGradParameters(epochs = 5, learningRate = 0.1)


  println("Training")
  val linearChain = LinearChain.train(input zip output, labels, params)
  println("Trained.")

  //println(linearChain.classify(input.head))

}