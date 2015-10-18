package ml.wolfe.examples

import ml.wolfe.nlp.Sentence
import ml.wolfe.nlp.io.CoNLLReader

/**
 * Created by luke on 20/05/15.
 */
object CoNLL2000 {
  def getChunkTags(s:Sentence) = {
    val chunkTags = Array.fill(s.tokens.length)("O")
    s.ie.entityMentions.getOrElse(IndexedSeq.empty).foreach { chunk =>
      chunkTags(chunk.start) = if (chunk.label == "O") "O" else "B-" + chunk.label
      for (i <- chunk.start+1 until chunk.end) chunkTags(i) = "I-" + chunk.label
    }
    IndexedSeq(chunkTags:_*)
  }

  def train() = {
    val data = new CoNLLReader("../wolfe/wolfe-examples/src/main/resources/ml/wolfe/datasets/conll2000/train.txt", " ")
    data.map(s => (s.tokens.map(_.word), getChunkTags(s)))
  }

  def test() = {
    val data = new CoNLLReader("../wolfe/wolfe-examples/src/main/resources/ml/wolfe/datasets/conll2000/test.txt", " ")
    data.map(s => (s.tokens.map(_.word), getChunkTags(s)))
  }
}