package ml.wolfe

import ml.wolfe.util.{NLP, MentionEvaluator}
import NLP._

/**
 * Created by luke on 12/06/14.
 */
class EvaluationSpecs extends WolfeSpec {
  def b = Token("", chunk = "B-something")
  def i = Token("", chunk = "I-something")
  def o = Token("", chunk = "O")

  "Mention Evaluator " should {
    "treat BIII as a single mention" in {
      def s = Sentence(Seq(b, i, i, i))
      MentionEvaluator.collectMentions(s, _.chunk) should have size 1
    }
    "treat BBBB as distinct mentions" in {
      def s = Sentence(Seq(b, b, b, b))
      MentionEvaluator.collectMentions(s, _.chunk) should have size 4
    }
    "treat I after O as a new mention" in {
      def s = Sentence(Seq(i, o, i, o, i))
      MentionEvaluator.collectMentions(s, _.chunk) should have size 3
    }
    "recognize sentences with no mentions" in {
      def s1 = Sentence(Seq(o, o, o, o, o))
      MentionEvaluator.collectMentions(s1, _.chunk) should have size 0
      def s2 = Sentence(Seq())
      MentionEvaluator.collectMentions(s2, _.chunk) should have size 0
    }

    "evaluate using *complete* mentions" in {
      def t = Seq(Sentence(Seq(b, o)))
      def g = Seq(Sentence(Seq(b, i)))
      def evaluation = MentionEvaluator.evaluate(t, g, _.chunk)
      evaluation.precision should equal (0)
    }

    "return correct evaluation stats" in {
      def t = Seq(Sentence(Seq(b, b, o, o)), Sentence(Seq(b, i, i, b)))
      def g = Seq(Sentence(Seq(b, o, o, b)), Sentence(Seq(b, i, i, o)))
      def evaluation = MentionEvaluator.evaluate(t, g, _.chunk)

      evaluation.tp shouldEqual 2
      evaluation.fp shouldEqual 1
      evaluation.fn shouldEqual 2

      evaluation.totalGold shouldEqual 4
      evaluation.totalGuess shouldEqual 3

      evaluation.precision.toFloat shouldEqual 2f/3
      evaluation.recall.toFloat shouldEqual 1f/2
      evaluation.f1.toFloat shouldEqual 4f/7
    }
  }
}
