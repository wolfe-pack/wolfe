package ml.wolfe.nlp.io

/**
 * @author narad
 * @author mbosnjak
 */
class MCTestReader(tsvFilename: String, ansFilename: String, rteFilename: String = null) extends Iterable[MultiQuestion] {
  private val MC_LABELS = Array("A", "B", "C", "D")
  private val RTE_REGEX = """.*<h>([^<]*)</h>.*""".r

  def iterator: Iterator[MultiQuestion] = {
    val tsvReader = io.Source.fromFile(tsvFilename).getLines()
    val ansReader = io.Source.fromFile(ansFilename).getLines()
    val zippedReaders = tsvReader.zip(ansReader)
    var rteq = if (rteFilename != null) {
      io.Source.fromFile(rteFilename).getLines.collect { case RTE_REGEX(rq) =>
        rq.replaceAll("&#39;", "'").replaceAll("&quot;", "\"").replaceAll("&amp;", "&")
      }.toList
    }
    else {
      List[String]()
    }

    zippedReaders.map { x =>
      val l = x._1
      val ans = x._2.split("\\s")
      val fields = l.split("\t")
      val id = fields(0)
      val author = fields(1)
      val passage = fields(2).replaceAll("\\\\newline", " ")
      val questions = fields.slice(3, fields.size).grouped(5).zip(ans.toIterator).map { case (g, t) =>
        val gi = g.head.split(": ")
        val (qt, q) = (gi(0), gi(1))
        val as = g.tail.zipWithIndex.map { case (a, i) =>
          if (rteq.isEmpty) {
            AnswerChoice(MC_LABELS(i), a, t == MC_LABELS(i))
          }
          else {
            val ta = rteq.head
            rteq = rteq.tail
            AnswerChoice(MC_LABELS(i), ta, t == MC_LABELS(i))
          }
        }
        new MultipleChoiceQuestion(q, as, qt)
      }


                      .toIterable
      MultiQuestion(id, author, passage, questions)
    }
  }
}

object MCTestReader {

  def main(args: Array[String]) {
    val tsvFilename = args.lift(0).getOrElse("../mctest/data/MCTest/mc160.dev.tsv")
    val ansFilename = args.lift(1).getOrElse("../mctest/data/MCTest/mc160.dev.ans")
    val rteFilename = args.lift(2).getOrElse(null)
    for (q <- new MCTestReader(tsvFilename, ansFilename, rteFilename)) {
      println(q + "\n")
    }
  }
}


class AristoReader(filename: String) extends Iterable[MultipleChoiceQuestion] {
  def iterator: Iterator[MultipleChoiceQuestion] = {
    val reader = io.Source.fromFile(filename).getLines().drop(28)
    reader.map{ x =>
      val fields = x.split("\t")
      val question = fields(9)
      val answer = fields(3)
      val pattern = "(?m)(\\([A-Z]\\)|$)".r
      val positions = pattern
                .findAllMatchIn(question)
                .map(_.start)
                .sliding(2)
                .toList
      val position_first = positions(0)(0)
      val answers = positions.map(x => question.slice(x(0), x(1)))
      val choices = answers.map{ a =>
        val label = pattern.findFirstIn(a).get
        val text = pattern.replaceFirstIn(a, "").trim
        AnswerChoice(label.charAt(1).toString, text, label.contains(answer))
      }.toIndexedSeq
      MultipleChoiceQuestion(question.substring(0, position_first).trim, choices, "")
    }
  }
}


object AristoReader extends App {
  val filename = args.lift(0).getOrElse("Ariscienceexams.txt")
    for (q <- new AristoReader(filename)) {
      println(q + "\n")
    }
}

case class MultiQuestion(id: String, author: String, passage: String, questions: Iterable[MultipleChoiceQuestion]) {

  override def toString = {
    passage + "\n" + questions.mkString("\n")
  }
}


//abstract case class Question(text: String) {
//
//  def isCorrect(str: String): Boolean
//}

case class MultipleChoiceQuestion(text: String, choices: IndexedSeq[AnswerChoice], val typ: String) {

  def isCorrect(label: String): Boolean = {
    choices.exists { c => c.label == label && c.isCorrect}
  }

  override def toString = {
    (Array(text) ++ choices.map { a =>
      " (%s) [%s]\t%s".format(a.label, if (a.isCorrect) "X" else " ", a.text)
    }).mkString("\n")
  }
}

case class AnswerChoice(label: String, text: String, isCorrect: Boolean)


// Temp location for some helper IO

import ml.wolfe.nlp.converters.SISTAProcessors

import scala.collection.mutable.ArrayBuffer

object DataToParseable {

  def main(args: Array[String]) { read(args(0), args(1), args(2)) }

  def read(f1: String, f2: String, f3: String): Array[String] = {
    val sb = new ArrayBuffer[String]
    val input = new MCTestReader(f1, f2, f3)

    for (i <- input) {
      val doc = SISTAProcessors.annotate(i.passage)
      for (s <- doc.sentences) {
        sb += (s.tokens.map(_.word).mkString(" "))
      }
      for (q <- i.questions) {
        sb += q.text // println(q.text)
        for (c <- q.choices) println(c.text)
      }
      //     sb += "" // println
    }
    println(sb.mkString("\n"))
    sb.toArray
  }
}



