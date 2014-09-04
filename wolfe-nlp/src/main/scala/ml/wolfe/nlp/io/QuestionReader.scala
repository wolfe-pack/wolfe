package ml.wolfe.nlp.io

/**
 * Created by narad on 9/3/14.
 */
class MCTestReader(filename: String) extends Iterable[MultiQuestion] {
  private val MC_LABELS = Array("A", "B", "C", "D")

  def iterator: Iterator[MultiQuestion] = {
    val reader = io.Source.fromFile(filename).getLines()
    reader.map{ l =>
      val fields = l.split("\t")
      val id = fields(0)
      val author = fields(1)
      val passage = fields(2).replaceAll("\\\\newline", " ")
      val questions = fields.slice(3, fields.size).grouped(5).map { g =>
        val gi = g.head.split(": ")
        val (qt, q) = (gi(0), gi(1))
        val as = g.tail.zipWithIndex.map { case(a,i) =>
          AnswerChoice(MC_LABELS(i), a, false)
        }
        new MultipleChoiceQuestion(q, as)
      }.toIterable
      MultiQuestion(id, author, passage, questions)
    }
  }
}

object MCTestReader {

  def main(args: Array[String]) {
    for (q <- new MCTestReader(args(0))) {
      println(q) + "\n"
    }
  }
}

case class MultiQuestion(id: String, author: String, passage: String, questions: Iterable[MultipleChoiceQuestion]) {

  override def toString = {
    passage + "\n" + questions.mkString("\n")
  }
}


abstract case class Question(text: String) {

  def isCorrect(str: String): Boolean
}

class MultipleChoiceQuestion(text: String, choices: Seq[AnswerChoice]) extends Question(text) {

  def isCorrect(label: String): Boolean = {
    choices.exists{ c => c.label == label && c.isCorrect}
  }

  override def toString = {
    (Array(text) ++ choices.map { a =>
      " (%s) [%s]\t%s".format(a.label, if (a.isCorrect) "X" else " ", a.text)
    }).mkString("\n")
  }
}

case class AnswerChoice(label: String, text: String, isCorrect: Boolean)