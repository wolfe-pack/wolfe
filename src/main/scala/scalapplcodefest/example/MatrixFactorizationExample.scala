package scalapplcodefest.example

import scalapplcodefest.{Index, TermDSL}
import scalapplcodefest.value.{Strings, Ints, Vectors}
import scalapplcodefest.term.Predicate

/**
 * User: rockt
 * Date: 12/28/13
 * Time: 4:56 PM
 */

object MatrixFactorizationExample extends App {

  import TermDSL._

  val key = new Index()

  //dimension of embeddings
  val k = 'k of ints

  //weights that need to get learned
  val w = 'w of vectors

  //fetch ith value in v and put it into a basis vector with the value at the ith state and zeros otherwise
  val project = for ((v, i) <- c(vectors, ints)) yield unit(i) * (unit(i) dot v)

  //TODO: I need to read that from data
  val Entities = set("e1", "e2", "e3")
  val Relations = set("r1", "r2")

  val rPredicate = 'rPredicate of c(strings, Entities, Entities) |-> bools

  //relation embeddings
  val a = for (r <- Relations as 'r) yield vectors.sum(for (i <- 0 ~~ k) yield project(w, key(r, i)))

  //tuple embeddings
  val v = for ((e1, e2) <- c(Entities, Entities) as ('e1, 'e2)) yield
    vectors.sum(for (i <- 0 ~~ k) yield project(w, key(e1, e2, i)))

  val model = for ((r, e1, e2) <- c(Relations, Entities, Entities) as ('r, 'e1, 'e2)) yield
    (a(r) dot v(e1, e2)) * I(rPredicate(r, e1, e2))

  println(model)

  //let's set some weights
  //TODO

  //TODO: objective
  //TODO: learning
}