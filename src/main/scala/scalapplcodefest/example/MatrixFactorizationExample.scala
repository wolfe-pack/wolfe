package scalapplcodefest.example

import scalapplcodefest.{Index, TermDSL}
import scalapplcodefest.value.{Ints, Vectors}

/**
 * User: rockt
 * Date: 12/28/13
 * Time: 4:56 PM
 */

class MatrixFactorizationExample {
  import TermDSL._

  val key = new Index()
  //embedding dimension
  val k = 'k of ints

  //weights that need to get learned
  val w = 'w of vectors

  //fetch ith value in v and put it into an basis vector with the value at the ith state and zeros otherwise
  val project = for ((v,i) <- c(vectors, ints)) yield unit(i) * (unit(i) dot v)

  val Rels = set("r1", "r2") //TODO: need to read that from data

  val a = for (r <- Rels) yield vectors.sum(for (i <- 0 ~~ k) yield project(w, key(r, i)))

  //val v = for ((x,y) <- Ents x Ents) yield vsum(for (i <- 0 ~~ 50) yield (unit(x,y,i) dot w) * unit(x,y,i))

  //val model = for ((r,x,y) <- C(…)) yield (a(r) dot v(x,y)) * I (r(x,y))

  //for (r,x,y) yield vsum( for (i <- 0 ~~ 50) unit(r,i)
}