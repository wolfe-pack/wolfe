package scalapplcodefest.example

import scalapplcodefest.{Index, TermDSL}
import scalapplcodefest.value.{Reduce, Strings, Ints, Vectors}
import scalapplcodefest.term._
import cc.factorie.la.{DenseTensor1, Tensor1}
import scalapplcodefest.term.Constant

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
  TermDebugger(query)

  //TODO: objective
  // || I(kbPred(r,e1,e2)) - (model | learned | kb(r,e1,e2) -> true) ||^2


  //TODO: learning
}

object TermDebugger {
  def apply(term: Term[_]) = {
//    try term.value() catch {
//      case e: NoSuchElementException =>
//        System.err.println(e)
//        System.err.println("========== Model ==========")
//        System.err.println(TermToDebugString(term))
//        System.err.println("===========================")
//        System.err.println(e.getStackTraceString)
//    }

    var problem: Term[_] = term

    def crashes(probe: Term[_]): Boolean = {
      var result = false
      try probe.value() catch {
        case e: Exception => result = true
        case _ => result = false
      }
      result
    }

    def check(probe: Term[_]) = if (crashes(probe)) problem = probe

    def diagnose(probe: Term[_], depth: Int = 0): Unit = {
      check(probe)
      probe match {
        case t: Predicate[_, _] => //nothing to do
        case t: Var[_] => //nothing to do
        case t: LambdaAbstraction[_, _] =>
          diagnose(t.sig)
          diagnose(t.body)
        case t: FunApp[_, _] =>
          diagnose(t.function)
          diagnose(t.arg)
        case t: TupleTerm2[_, _] =>
          diagnose(t.a1)
          diagnose(t.a2)
        case t: TupleTerm3[_, _, _] =>
          diagnose(t.a1)
          diagnose(t.a2)
          diagnose(t.a3)
        case t: Reduce[_] =>
          diagnose(t.op)
          diagnose(t.arguments)
        case t: SeqTerm[_] =>
          t.componentSeq.foreach(diagnose(_))
        case t: Conditioned[_] =>
          t.componentSeq.foreach(diagnose(_))
        case t: Composite[_] =>
          t.componentSeq.foreach(diagnose(_))
        case t: Term[_] =>
          println("TODO: " + term.getClass)
        case _ => throw new NotImplementedError(s"I don't know yet how to debug a $term")
      }
      println("tracking problem: " + problem)
    }

    if (crashes(term)) {
      println(TermToDebugString(term))
      diagnose(term)
    }
  }
}

object TermToDebugString {
  def apply(term: Term[_], indent: Int = 0): String = {
    def tabs = "\t" * indent

    term match {
      case t: LambdaAbstraction[_, _] => s"\n${tabs}lam ${t.sig} { ${apply(t.body, indent + 1)} }"
      case t: FunApp[_, _] => s"${apply(t.function, indent)}(${apply(t.arg, indent)})"
      case t: TupleTerm2[_, _] => s"(${apply(t.a1, indent)}, ${apply(t.a2, indent)})"
      case t: TupleTerm3[_, _, _] => s"(${apply(t.a1, indent)}, ${apply(t.a2, indent)}, ${apply(t.a3, indent)})"
      case t: Var[_] => t.toString
      case t: Reduce[_] => s"Reduce(${apply(t.op, indent)})(${apply(t.arguments, indent)})"
      case t: SeqTerm[_] => s"${t.seq.map(apply(_, indent)).mkString("[",",","]")}"
      case t: Predicate[_, _] => s"${t.toString}"
      case t: Conditioned[_] => s"C:${t.componentSeq.map(apply(_, indent)).mkString(",")} \n| ${t.condition}"
      case t: Composite[_] => s"${t.getClass.getSimpleName} ${t.componentSeq.map(apply(_, indent)).mkString(",")}"
      case t: Term[_] =>
        //println("TODO: " + term.getClass)
        term.toString
      case _ => throw new NotImplementedError(s"I don't know yet how to prettify the output of a $term")
    }
  }
}