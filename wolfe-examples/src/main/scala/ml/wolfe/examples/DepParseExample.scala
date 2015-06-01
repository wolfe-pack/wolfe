package ml.wolfe.examples

import ml.wolfe.SimpleIndex
import ml.wolfe.term.TermImplicits._
import ml.wolfe.term._

/**
 * @author riedel
 */
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

  val theta = zeros + weight('bias, 1.0, true)

  implicit val params = BPParameters(2, BP.Schedule.synchronized)

  val margs = marginals(Parses) { y => model(theta, s1.toConst, y) marginalsBy Marginalizer.sumProduct }
//
  println(theta.eval())
  println(margs.eval())


}



