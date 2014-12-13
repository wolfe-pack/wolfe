package ml.wolfe.util

import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom

/**
 * @author rockt
 */
object RichCollections {
  //from: http://stackoverflow.com/questions/3912753/scala-remove-duplicates-in-list-of-objects
  class RichCollection[A, Repr](xs: IterableLike[A, Repr]){
    def distinctBy[B, That](f: A => B)(implicit cbf: CanBuildFrom[Repr, A, That]) = {
      val builder = cbf(xs.repr)
      val i = xs.iterator
      var set = Set[B]()
      while(i.hasNext) {
        val o = i.next()
        val b = f(o)
        if (!set(b)) {
          set += b
          builder += o
        }
      }
      builder.result()
    }
  }

  implicit def toRich[A, Repr](xs: IterableLike[A, Repr]) = new RichCollection(xs)

  implicit class Crossable[X](xs: Traversable[X]) {
    def cross[Y](ys: Traversable[Y]) = for { x <- xs; y <- ys } yield Seq(x, y)
    def flatCross[Y](ys: Traversable[Y]) =
      for { x <- xs; y <- ys } yield x match {
        case ls: Seq[_] => ls ++ Seq(y)
        case _ => Seq(x, y)
      }
  }
}
