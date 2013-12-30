package scalapplcodefest.example

import scalapplcodefest.{Index, TermDSL}
import cc.factorie.la.DenseTensor1
import scalapplcodefest.term.{Term, State, Constant}
import scalapplcodefest.TermDebugger._

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

  //TODO: I need to read these from data
  val Entities = set("e1", "e2", "e3")
  val Relations = set("r1", "r2")

  //represents facts (r_l(e_m, e_n) = true) in the knowledge base
  val kbPred = 'kbPred of c(strings, Entities, Entities) |-> bools
  val kb = state(
    kbPred.atom("r1", "e1", "e2") -> true,
    kbPred.atom("r2", "e1", "e3") -> true,
    kbPred.atom("r1", "e1", "e3") -> false
  )

  //relation embeddings
  val relE = for (r <- Relations as 'r) yield vectors.sum(for (i <- 0 ~~ k) yield project(w, key(r, i)))

  //tuple embeddings
  val tupE = for ((e1, e2) <- c(Entities, Entities) as ('e1, 'e2)) yield
    vectors.sum(for (i <- 0 ~~ k) yield project(w, key(e1, e2, i)))

  val model = for ((r, e1, e2) <- c(Relations, Entities, Entities) as ('r, 'e1, 'e2)) yield
    (relE(r) dot tupE(e1, e2)) * I(kbPred(r, e1, e2))

  val learned = state(
    k -> 2, //embeddings are vectors with two components
    w -> new DenseTensor1(Array(1.0, 2.0, 0.0, 1.0)) //setting weights, later these will get learned from data
  )

  val predict = model | learned | kb

  //rockt: working
  //val query = predict(Constant("r1"), Constant("e1"), Constant("e2"))
  //println("score of r1(e1,e2): " + query.value())


  val query = predict(Constant("r2"), Constant("e1"), Constant("e2"))

  debugTerm(query)

  //TODO: objective
  // || I(kbPred(r,e1,e2)) - (model | learned | kb(r,e1,e2) -> true) ||^2

  //TODO: learning
}