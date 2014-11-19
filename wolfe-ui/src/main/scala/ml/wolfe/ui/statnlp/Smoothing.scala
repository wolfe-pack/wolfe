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

  def mygreen = "#58C554"//  "#78AB46"

  def span(color: String)(body: String) = s"""<span style="color: $color">$body</span>"""


  section("ZipfGraph") {

    md("##Counts from ACL")

    wolfe(
      """
        |val file = new java.io.File("/Users/sriedel/projects/stat-nlp-course-intern/data/counts.json")
        |val counts = io.IO.loadTSV(file)(a => a(0) -> a(1).toInt)
        |val maxCount = counts.head._2
        |counts.take(10)
      """.stripMargin)
  }

  section("Tail") {

    md("##The Long Tail")

    wolfe(
      """
        |val x = counts.indices.steps(1000).map(_ + 1.0)
        |val Y1 = Y(counts.steps(1000).map(_._2).map(_.toDouble),"counts")
        |val chart = plot((x,(Y1)),x=Axis("rank",log=false),y=Axis("freq",log=false))
        |D3Plotter.lineplot(chart)
      """.stripMargin
    )

  }

  section("Zipf") {

    md(
      s"""
        |## Zipf's Law
        |The count ${ span(mygreen)("$f$") } of a word is inversely proportional to its rank ${ span(mygreen)("$r$") }.
      """.stripMargin)

    latex("f(r) = \\frac{M}{r}")
  }

  section("ZipfD3") {

    md("##Zipf vs Counts")

    wolfe(
      """
        |def zipf(a:Double)(r:Double) = maxCount / math.pow(r,a)
        |val Y2 = Y(x map (zipf(1.0)),"zipf")
        |val chart2 = plot((x,(Y1,Y2)),x=Axis("rank",log=true),y=Axis("freq",log=true))
        |D3Plotter.lineplot(chart2)
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

  val aclDir     = new File("/Users/sriedel/corpora/cleaned_acl_arc_ascii")
  val countsFile = new File("/Users/sriedel/projects/stat-nlp-course-intern/data/counts.json")

  def main(args: Array[String]) {

    val pipeline = TokenSplitter andThen SentenceSplitter
    val files = Util.files(aclDir) filter (_.getName.startsWith("P"))
    val docs = loadTxts(files) map pipeline

    println(docs.size)

    def calculateCounts(docs: Seq[Document]) = {
      type Counts = Map[String, Int]
      def words = for (d <- docs.view; s <- d.sentences.view; t <- s.tokens.view) yield t.word
      val init = Map.empty[String, Int] withDefaultValue 0
      def add(counts: Counts, word: String) = counts + (word -> (counts(word) + 1))
      words.foldLeft(init)(add)
    }

    val counts = calculateCounts(docs)

    println(counts.size)

    val sorted = counts.toSeq.sortBy(-_._2)
    println(sorted.take(10).mkString("\n"))

    IO.saveTSV(sorted, countsFile)(c => IndexedSeq(c._1, c._2.toString))

  }
}

object TestSmoothing {
  def main(args: Array[String]) {
    val counts = IO.loadTSV(PrepareSmoothing.countsFile)(a => a(0) -> a(1).toInt)
    println(counts.take(10).mkString("\n"))
  }
}