package ml.wolfe.examples

import ml.wolfe.SimpleIndex
import ml.wolfe.term.{Argmaxer, TermImplicits, TypedDom}

/**
 * @author riedel
 */
object LinearChain {

  def main(args: Array[String]) {

    import TermImplicits._

    val data = Seq((IndexedSeq("A"),IndexedSeq("NN")))

    implicit val W = vectors(10)
    implicit val Labels = Discretes("NN","VBD","IN")
    val index = new SimpleIndex

    case class Instance(x:IndexedSeq[String]) {
      val n = x.length
      val Y = seqs(Labels,n)
      def chain(w:W.Term)(y:Y.Term) = {
        sum(0 until n){ i => oneHot(index(x(i)->y(i))) dot w}
      }
      def negLoss(yGold:IndexedSeq[String])(w:W.Term) = {
        max(Y){chain(w)} - chain(w)(Y.Const(yGold))
      }
      def predict(w:W.Term) = {
        argmax(Y){chain(w)}
      }
    }

    def loss(w:W.Term) = sum(data) { case (x, y) => Instance(x).negLoss(y)(w)}.argmaxBy(Argmaxer.ascent(10))

    val w = argmax(W)(loss).eval()

    println(w)



  }


}
