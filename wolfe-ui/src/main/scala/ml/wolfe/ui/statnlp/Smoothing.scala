package ml.wolfe.ui.statnlp

import java.io.{File, PrintStream}

import ml.wolfe.nlp.io.IO
import ml.wolfe.ui.MutableMoroNotebook
import ml.wolfe.util.Util

import scala.io.Source

/**
 * @author Sebastian Riedel
 */
/**
 * @author Sebastian Riedel
 */
object Smoothing extends MutableMoroNotebook with App {

  def mygreen = "#78AB46"

  def span(color: String)(body: String) = s"""<span style="color: $color">$body</span>"""

  section("Zipf") {

    md(
      s"""
        |## Zipf's Law
        |The frequency ${span(mygreen)("$f$")} of a word is inversely proportional to its rank ${span(mygreen)("$r$")}.
      """.stripMargin)

    latex("f(k; s, N) = \\frac{1 / k^s}{ \\sum_i i}")
  }

  section("ZipfGraph") {

    md("##Counts from ACL")

    wolfe(
      """
        |import scala.io.Source
        |val file = "/Users/sriedel/projects/stat-nlp-course-intern/data/counts.json"
        |def toPair(split:Array[String]) = (split(0),split(1).toInt)
        |val lines = Source.fromFile(file).getLines()
        |val counts = lines.map(_.split("\t")).map(toPair).toList
        |counts.take(10)
      """.stripMargin)
  }

  section("Tail") {

    md("##The Long Tail")

    wolfe(
      """
        |def steps[T](col:Seq[T],step:Int) =
        |  (0 until col.length by step) map col
        |val x = steps(counts.indices,1000).map(_.toDouble)
        |val y = steps(counts,1000).map(_._2).map(_.toDouble)
        |val chart = plot((x,(Y(y,"freq"))),y=Axis("freq",log=true))
        |D3Plotter.lineplot(chart)        |
      """.stripMargin
    )

  }

  saveTo("Smoothing", new File(Slides.dir, "smoothing.json"))


}


object Slides {
  val dir = new File("/Users/sriedel/projects/moro-notebooks/stat-nlp")
  dir.mkdirs()

}

object PrepareSmoothing {

  import ml.wolfe.nlp._

  val aclDir = new File("/Users/sriedel/corpora/cleaned_acl_arc_ascii")
  val countsFile = new File("/Users/sriedel/projects/stat-nlp-course-intern/data/counts.json")

  def main(args: Array[String]) {

    val pipeline = TokenSplitter andThen SentenceSplitter
    val files = Util.files(aclDir) filter (_.getName.startsWith("P"))
    val docs = loadTxts(files) map pipeline

    println(docs.size)

    def calculateCounts(docs:Seq[Document]) = {
      type Counts = Map[String,Int]
      def words = for (d <- docs.view; s <- d.sentences.view; t <- s.tokens.view) yield t.word
      val init = Map.empty[String,Int] withDefaultValue 0
      def add(counts:Counts, word:String) = counts + (word -> (counts(word) + 1))
      words.foldLeft(init)(add)
    }

    val counts = calculateCounts(docs)

    println(counts.size)

    val sorted = counts.toSeq.sortBy(-_._2)
    println(sorted.take(10).mkString("\n"))

    IO.saveTSV(sorted, countsFile)(c => Seq(c._1,c._2.toString))

  }
}

object TestSmoothing {
  def main(args: Array[String]) {
    val file = "/Users/sriedel/projects/stat-nlp-course-intern/data/counts.json"
    def toPair(split:Array[String]) = (split(0),split(1))
    val lines = Source.fromFile(file).getLines()
    val counts = IO.loadTSV(PrepareSmoothing.countsFile)(a => a(0) -> a(1).toInt)
    println(counts.take(10).mkString("\n"))
  }
}