package ml.wolfe.nlp

import ml.wolfe.WolfeSpec
import ml.wolfe.nlp.syntax.{DependencyTree, Arc}

/**
 * Created by narad on 07/09/15.
 */
class TreeSpecs extends WolfeSpec {

  val tokens = IndexedSeq(
    Token(word = "the", offsets = CharOffsets(0,1)),
    Token(word = "cat", offsets = CharOffsets(1,2)),
    Token(word = "scratched", offsets = CharOffsets(2,3)),
    Token(word = "the", offsets = CharOffsets(3,4)),
    Token(word = "man", offsets = CharOffsets(4,5)),
    Token(word = "with", offsets = CharOffsets(5,6)),
    Token(word = "claws", offsets = CharOffsets(6,7)))
  val arcs = Seq(
    Arc(1,0, Some("DET")),
    Arc(2,1, Some("SUBJ")),
    Arc(4,3, Some("DET")),
    Arc(2,4, Some("OBJ")),
    Arc(2,5, Some("PREP")),
    Arc(5,6, Some("PRP")))
  val tree = DependencyTree(tokens, arcs)

  "A dependency tree" should {
    "find the correct shortest path" in {
      val path = tree.shortestPath(0, 6).get
      val result = path.map(_.source) ++ Seq(path.last.dest)
      val correct = Seq(0, 1, 2, 5, 6)
      result should equal(correct)
    }

    "find the correct shortest directed path" in {
      val path = tree.shortestPath(2, 0).get
      val result = path.map(_.source) ++ Seq(path.last.dest)
      val correct = Seq(2, 1, 0)
      result should equal(correct)
    }

    "identify when there is not a directed path" in {
      val result = tree.shortestDirectedPath(0, 6)
      result should equal(None)
    }
  }
}
