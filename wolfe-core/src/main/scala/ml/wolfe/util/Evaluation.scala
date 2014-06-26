package ml.wolfe.util

import ml.wolfe.util.NLP.{Label, Sentence, Token}
import scala.collection.mutable.ListBuffer

case class EvaluationSummary(evaluations:Map[Any,Evaluation]) {
  override def toString = {
    val keys = evaluations.keys.toSeq.sortBy(_.toString)
    val lines = for (key <- keys.view) yield
      f"""
        |------------
        |Key:         $key
        |${evaluations(key)}
      """.stripMargin
    lines.mkString("\n")

  }
}

case class Evaluation(tp: Int = 0, tn: Int = 0, fp: Int = 0, fn: Int = 0) {
  def totalGold = tp + fn
  def totalGuess = tp + fp
  def precision = tp.toDouble / totalGuess
  def recall = tp.toDouble / totalGold
  def f1 = 2.0 * precision * recall / (precision + recall)
  def +(that: Evaluation) = Evaluation(tp + that.tp, tn + that.tn, fp + that.fp, fn + that.fn)
  override def toString =
    f"""Total Gold:  $totalGold
      |Total Guess: $totalGuess
      |Precision:   $precision%f
      |Recall:      $recall%f
      |F1:          $f1%f
    """.stripMargin
}

object Evaluator {
  def evaluate[T](target: Iterable[T], guess: Iterable[T])(attribute: T => Any): Evaluation = {
    val evaluations = for ((t, g) <- target.view zip guess.view) yield evaluate(t, g)(attribute)
    val reduced = evaluations.foldLeft(Evaluation())(_ + _)
    reduced
  }

  def evaluate[T](target: T, guess: T)(attribute: T => Any): Evaluation =
    if (attribute(target) == attribute(guess)) Evaluation(tp = 1, tn = 1) else Evaluation(fp = 1, fn = 1)

}



case class Mention(start: Int, end: Int, label: String) {
  def inc = Mention(start, end + 1, label)
}

object MentionEvaluator {
  import scala.language.implicitConversions

  def evaluate[T <: Sentence](target: Iterable[T], guess: Iterable[T], getLabel: Token=>Label = _.tag): Evaluation = {
    val evaluations = for ((t, g) <- target.view zip guess.view) yield
      evaluateMentions( collectMentions(t, getLabel), collectMentions(g, getLabel) )
    val reduced = evaluations.foldLeft(Evaluation())(_ + _)
    reduced
  }

  implicit def collectMentions[T <: Sentence](t: T, getLabel: Token=>Label): Set[Mention] = {
    t.tokens.zipWithIndex.foldLeft(ListBuffer[Mention]())((mentions: ListBuffer[Mention], tokenWithIndex) => {
      val (token, ix) = tokenWithIndex
      val label = getLabel(token).label.name
      val Array(prefix, labelType) = if (label == "O") Array("O", "O") else label.split("-")

      prefix match {
        case "O" => mentions
        case "B" => mentions :+ Mention(ix, ix, labelType)
        case "I" =>
          if (mentions.isEmpty) mentions :+ Mention(ix, ix, labelType)
          else {
            val last = mentions.last
            if (last.end == ix - 1 && last.label == labelType) mentions.updated(mentions.length - 1, mentions.last.inc)
            else mentions :+ Mention(ix, ix, labelType)
          }
    }}).toSet
  }

  def evaluateMentions(target: Set[Mention], guess: Set[Mention]): Evaluation = {
    val tp = (target intersect guess).size
    val fp = (guess diff target).size
    val fn = (target diff guess).size
    Evaluation(tp, 0, fp, fn)
  }
}
