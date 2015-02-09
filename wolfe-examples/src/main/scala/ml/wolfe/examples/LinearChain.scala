package ml.wolfe.examples

import ml.wolfe.SimpleIndex
import ml.wolfe.term.{TermImplicits, TypedDom}

/**
 * @author riedel
 */
object LinearChain {

  def main(args: Array[String]) {

    import TermImplicits._

    type Input = TypedDom[IndexedSeq[String]]

    implicit val W = vectors(2)

    val n = 4
    val Labels = discrete("NN","VBD","IN")
    val Y = seqs(Labels,n)
    val index = new SimpleIndex

    def chain(w:W.Term)(x:IndexedSeq[String])(y:Y.Term) = {
      sum(0 until n){ i => oneHot(index(x(i)->y(i))) dot w}
    }

    def loss(x:IndexedSeq[String], yGold:IndexedSeq[String])(w:W.Term) = {
      max(Y){chain(w)(x)} - chain(w)(x)(Y.const(yGold))
    }

    def predict(x:IndexedSeq[String])(w:W.Term) = {
      argmax(Y){chain(w)(x)}
    }

  }


}
