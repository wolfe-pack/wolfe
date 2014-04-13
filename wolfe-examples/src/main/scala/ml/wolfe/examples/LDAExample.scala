package ml.wolfe.examples

import ml.wolfe.Wolfe

/**
 * @author Sebastian Riedel
 */
object LDAExample {
  import Wolfe._
  case class Token(topic:Int,word:String)
  case class Doc(theta:Map[Int,Double],tokens:Seq[Token])
  case class Data(phi:Seq[Map[String,Double]],docs:Seq[Doc])

  def prod[T](dom:Iterable[T])(p:T => Double) = dom.map(p).product

  def dir[T](dist:Map[T,Double],beta:Double):Double = 0.0
  def cat[T](c:T,dist:Map[T,Double]):Double = 0.0


def lda(w:Vector)(d:Data) = {
  import d._
  prod(phi) { dir(_,w('beta))} *
  prod(docs) { d =>
    dir(d.theta, w('alpha)) *
    prod(d.tokens) {t =>
      cat(t.topic, d.theta) *
      cat(t.word, phi(t.topic))
    }
  }

}

}
