package scalapplcodefest

import scalapplcodefest.term._
import scalapplcodefest.term.LambdaAbstraction
import scalapplcodefest.value.Reduce
import scalapplcodefest.term.Var
import scalapplcodefest.term.TupleTerm2
import scalapplcodefest.term.Conditioned
import scalapplcodefest.term.TupleTerm3
import scalapplcodefest.term.Predicate
import scalapplcodefest.term.FunApp

/**
 * User: rockt
 * Date: 12/30/13
 * Time: 4:55 PM
 */

object TermDebugger {
  def debugTerm(term: Term[_], verbose: Boolean = false) = {
    var problem: Term[_] = term

    def crashes(probe: Term[_]): Boolean = {
      try probe.value() catch {
        case e: Exception => return true
      }
      false
    }

    def check(probe: Term[_]) = if (crashes(probe)) problem = probe

    def diagnose(probe: Term[_]): Unit = {
      probe match {
        case t: Predicate[_, _] => check(probe)
        case t: Var[_] => //nothing to do
        case t: FunApp[_, _] =>
          check(probe)
          if (!t.function.isInstanceOf[Predicate[_, _]]) t.componentSeq.foreach(diagnose)
        case t: Conditioned[_] =>
          check(probe)
          t.componentSeq.foreach(diagnose)
        case t: Composite[_] =>
          check(probe)
          t.componentSeq.foreach(diagnose)
        case t: Term[_] =>
          //println("TODO: " + term.getClass)
          check(probe)
        case _ => throw new NotImplementedError(s"I don't know yet how to debug a $term")
      }
      if (verbose && crashes(probe)) println("problem: " + problem)
    }

    try term.value() catch {
      case e: NoSuchElementException =>
        diagnose(term)
        System.err.println(e)
        System.err.println("- problem\n\t" + problem)
        System.err.println("========== Model ==========")
        System.err.println(termToPrettyString(term))
        System.err.println("===========================")
        System.err.println(e.getStackTraceString)
    }
  }

  def termToPrettyString(term: Term[_], indent: Int = 0): String = {
    def tabs = "\t" * indent
    term match {
      case t: LambdaAbstraction[_, _] => s"\n${tabs}lam ${t.sig} { ${termToPrettyString(t.body, indent + 1)} }"
      case t: FunApp[_, _] =>
        if (indent == 0) s"- function application of\n\t${termToPrettyString(t.arg, indent + 1)}\n- applied to${termToPrettyString(t.function, indent + 1)}"
        else s"${termToPrettyString(t.function, indent)}(${termToPrettyString(t.arg, indent)})"
      case t: TupleTerm2[_, _] => s"(${termToPrettyString(t.a1, indent)}, ${termToPrettyString(t.a2, indent)})"
      case t: TupleTerm3[_, _, _] => s"TupleTerm3(${termToPrettyString(t.a1, indent)},${termToPrettyString(t.a2, indent)},${termToPrettyString(t.a3, indent)})"
      case t: Var[_] => t.toString
      case t: Reduce[_] => s"Reduce(${termToPrettyString(t.op, indent)})(${termToPrettyString(t.arguments, indent)})"
      case t: SeqTerm[_] => s"${t.seq.map(termToPrettyString(_, indent)).mkString("[",",","]")}"
      case t: Predicate[_, _] => s"${t.toString}"
      case t: Conditioned[_] => s"${t.componentSeq.map(termToPrettyString(_, indent)).mkString(",")}\n- conditioned on\n\t${t.condition}"
      case t: Composite[_] => s"${t.getClass.getSimpleName} ${t.componentSeq.map(termToPrettyString(_, indent)).mkString(",")}"
      case t: Term[_] =>
        //println("TODO: " + term.getClass)
        term.toString
      case _ => throw new NotImplementedError(s"I don't know yet how to prettify the output of a $term")
    }
  }
}