package scalapplcodefest.example

import scalapplcodefest.{Index, TermDSL}
import scalapplcodefest.value.{Ints, Vectors}

/**
 * User: rockt
 * Date: 12/28/13
 * Time: 4:56 PM
 */

object MatrixFactorizationExample extends App {
  import TermDSL._

  val key = new Index()

  //embedding dimension
  val k = 'k of ints

  //weights that need to get learned
  val w = 'w of vectors

  //fetch ith value in v and put it into an basis vector with the value at the ith state and zeros otherwise
  val project = for ((v,i) <- c(vectors, ints)) yield unit(i) * (unit(i) dot v)

  //TODO: need to read that from data
  val Entities = set("e1", "e2", "e3")
  //TODO: these guys should be "real" relations, e.g., a predicate depending on relation name and two entities
  val Relations = set("r1", "r2")

  val a = for (r <- Relations) yield vectors.sum(for (i <- 0 ~~ k) yield project(w, key(r, i)))

  val v = for ((x,y) <- c(Entities, Entities)) yield vectors.sum(for (i <- 0 ~~ k) yield unit(key(x,y,i)) * (unit(key(x,y,i)) dot w))

  //FIXME
  val model = for ((r,x,y) <- c(Relations, Entities, Entities)) yield (a(r) dot v(x,y)) * I(r(x,y))
}