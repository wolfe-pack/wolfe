package ml.wolfe.examples

import ml.wolfe.SimpleIndex
import ml.wolfe.term.TermImplicits._
import ml.wolfe.term._
import org.sameersingh.htmlgen.ConverterUtils._
import org.sameersingh.htmlgen.Custom.Matrix
import org.sameersingh.htmlgen.{RawHTML, HTML}
import scala.language.implicitConversions

/**
 * @author riedel
 */
object DepParseHelper {
  def toMatrix(words:Seq[String],marginals:VarSeqDom[VarSeqDom[Bools.type]]#Marginals) = {
    val length = words.length//marginals.length.maxBy(_._2)._1
    val values = for (m <- 0 until length) yield for (h <- 0 until length) yield {
        val belief = marginals(m)(h)
        val prob = math.exp(belief(true)) / (math.exp(belief(true)) + math.exp(belief(false)))
        prob
      }
    println(values.mkString("\n"))
    Matrix(values, words, words)
  }

  implicit def matrix[M](m: Matrix[M]): HTML = {
    val indentLevel = 0
    val sb = new StringBuilder
    sb.append(indent(indentLevel) + "<table class=\"matrix\">\n")
    val cells = m.data.map(_.map(m extr _)).flatten
    val min = cells.min
    val max = cells.max
    val dimC = 250.0/(m.cols)
    val dimR = 250.0/(m.rows)
    def opacity(d: Double) = d
    sb.append(indent(indentLevel+1) + "<thead>\n")
    if(!m.colNames.isEmpty) {
      // column names
      sb.append(indent(indentLevel+1) + "<tr class=\"matrixRow\">\n")
      if(!m.rowNames.isEmpty)
        sb.append(indent(indentLevel+1) + "<th></th>\n")
      for(j <- 0 until m.cols) {
        sb.append(indent(indentLevel+2) + "<th class=\"rotate\"><div><span>%s</span></div></th>\n" format (m.colNames(j)))
      }
      sb.append(indent(indentLevel+1) + "</tr>\n")
    }
    sb.append(indent(indentLevel+1) + "</thead>\n")
    sb.append(indent(indentLevel+1) + "<tbody>\n")
    for(i <- 0 until m.rows) {
      sb.append(indent(indentLevel+1) + "<tr class=\"matrixRow\">\n") // style="width:100%%;height:%fpx" format(dimR)
      if(!m.rowNames.isEmpty)
        sb.append(indent(indentLevel+2) + "<th><div><span>%s</span></div></th>\n" format (m.rowNames(i)))
      for(j <- 0 until m.cols) {
        val o = opacity(m.cell(i,j))
        sb.append(indent(indentLevel+2) + "<td class=\"matrixCell\" style=\"opacity:%f\"/>\n" format(o)) //width:%fpx;height:100%%;

      }
      sb.append(indent(indentLevel+1) + "</tr>\n")
    }
    sb.append(indent(indentLevel+1) + "</tbody>\n")
    sb.append(indent(indentLevel) + "</table>\n")
    RawHTML(sb.mkString)
  }
}

object DepParseExample extends App {

  implicit val index = new SimpleIndex()

  val maxLength = 10
  val maxFeats = 1000

  @domain case class Sentence(word: IndexedSeq[String], pos: IndexedSeq[String])

  //type Parse = IndexedSeq[IndexedSeq[Boolean]]
  //implicit val Parses2 = Graphs(Ints(0 until maxLength), Ints(1 until maxLength))

  val s1 = Sentence(Vector("ROOT", "a", "cat", "sat", "on", "the", "mat"), Vector("ROOT", "DT", "NN", "VBD", "IN", "DT", "NN"))
  val sentences = Seq(s1)

  implicit val Thetas = Vectors(maxFeats)
  implicit val Words = sentences.flatMap(_.word).distinct.toDom
  implicit val Tags = sentences.flatMap(_.pos).distinct.toDom
  implicit val Sentences = Sentence.Objects(Seqs(Words, 0, maxLength), Seqs(Tags, 0, maxLength))
  implicit val Parses = Seqs(Seqs(Bools, 0, maxLength), 0, maxLength)

  def features(x: Sentences.Term, y: Parses.Term, head: IntTerm, mod: IntTerm) = {
    feature('bias, y(mod)(head)) +
      feature('pos_00, x.pos(head), x.pos(mod), y(mod)(head))
  }

  def linear(t: Thetas.Term, x: Sentences.Term, y: Parses.Term) = {
    sum(0 until x.word.length) { head =>
      sum(1 until x.word.length) { mod =>
        t dot features(x, y, head, mod)
      }
    }
  }

  def model(t: Thetas.Term, x: Sentences.Term, y: Parses.Term) =
    linear(t, x, y) subjectTo projectiveTree(y, x.word.length)

  val theta = zeros + weight('bias, 1.0, true) + weight('pos_00, 1.0, Tags.Const("NN"), Tags.Const("DT"), true)

  implicit val params = BPParameters(2, BP.Schedule.synchronized)

  val margs = marginals(Parses) { y => model(theta, s1.toConst, y) marginalsBy Marginalizer.sumProduct }
  //
  println(theta.eval())

  val evalMargs = margs.eval()

  println(evalMargs)
  println(evalMargs(1)(2).expNormalize)
  println(evalMargs(2)(1).expNormalize)








}



