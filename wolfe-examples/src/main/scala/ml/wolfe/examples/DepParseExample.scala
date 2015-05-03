package ml.wolfe.examples

import ml.wolfe.SimpleIndex
import ml.wolfe.term.TermImplicits._
import ml.wolfe.term._

/**
 * @author riedel
 */
object DepParseExample {

  implicit val index = new SimpleIndex()

  val maxLength = 10
  val maxFeats = 10000

  @domain case class Sentence(word: IndexedSeq[String], pos: IndexedSeq[String])

  //type Parse = IndexedSeq[IndexedSeq[Boolean]]
  //implicit val Parses2 = Graphs(Ints(0 until maxLength), Ints(1 until maxLength))

  val example = Seq(
    Sentence(Vector("ROOT", "a", "cat", "sat", "on", "the", "mat"), Vector("ROOT", "DT", "NN", "VBD", "IN", "DT", "NN"))
      -> Map((3, 2) -> true)
  )

  implicit val Thetas = Vectors(maxFeats)
  implicit val Words = example.flatMap(_._1.word).distinct.toDom
  implicit val Tags = example.flatMap(_._1.pos).distinct.toDom
  implicit val Sentences = Sentence.Objects(Seqs(Words, 0, maxLength), Seqs(Tags, 0, maxLength))
  implicit val Parses = Seqs(Seqs(Bools, 0, maxLength), 0, maxLength)

  def features(x: Sentences.Term, y: Parses.Term, head: IntTerm, mod: IntTerm) = {
    feature('bias, y(head)(mod)) +
      feature('lex_00, x.word(head), x.word(mod), y(head)(mod))
  }

  def firstOrder(t: Thetas.Term, x: Sentences.Term, y: Parses.Term) = {
    sum(0 until x.word.length) { head =>
      sum(1 until x.word.length) { mod =>
        t dot features(x, y, head, mod)
      }
    } subjectTo (y.length === x.word.length)
  }

}

abstract class PTree(parse:Term[VarSeqDom[VarSeqDom[BooleanDom]]],slen:IntTerm) extends ComposedDoubleTerm {


  def copy(args: IndexedSeq[ArgumentType]) = ???

  def arguments = ???

  type ArgumentType = this.type

  override def maxMarginalizerImpl(wrt: Seq[AnyVar], observed: Seq[AnyVar])
                                  (input: Settings, inputMsgs: Msgs, reverseMsgsAlso: Boolean) = {
    new MaxMarginalizer {
      //require that the parse variable term is a sequence of sequence of boolean vars
      //require that wrt contains all variable (we don't do partial msgs right now
      //require that reverseMsgAlso is true (we want to get msgs on target vars)

      //require(parse.isInstanceOf[AnyVar])
      def maxMarginals()(implicit execution: Execution) = ???

      val input: Settings = ???
      val outputMsgs: Msgs = ???
      val inputMsgs: Msgs = ???
    }
  }
}


