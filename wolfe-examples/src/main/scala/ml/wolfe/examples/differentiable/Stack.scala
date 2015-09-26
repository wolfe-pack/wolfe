package ml.wolfe.examples.differentiable

import ml.wolfe.examples.differentiable.StackOps.Push
import ml.wolfe.term.simplified._
import org.scalautils.{Every, Or}

/**
 * @author riedel
 */
@term case class Stack[C](store: Seq[C], strength: Seq[Double])

object StackOps {

  import Wolfe._

  case class Push[C](stack:STerm[Stack[C]],push:STerm[Double],pop:STerm[Double],value:STerm[C]) extends STerm[Stack[C]]

  def push[C](stack: STerm[Stack[C]])(push: STerm[Double], pop: STerm[Double], value: STerm[C]) = {
    val store = stack.store :+ value
    val zeroes = fill(stack.strength.length)(0.0)
    val pops = fill(stack.strength.length)(pop)
    val accumulated = RangeTerm(0, stack.strength.length).map {
      i => stack.strength.slice(i + 1, stack.strength.length - 1).sum
    }
    val updatedStrength = max(zeroes, stack.strength - max(zeroes, pops - accumulated))
    val strength = updatedStrength :+ push
    Stack.Term(store, strength)
  }

  def read[C](stack: STerm[Stack[C]]) = {

  }

  def head[C](stack: STerm[Stack[C]]): STerm[C] = stack.store(0)

  def main(args: Array[String]) {

    @term case class LogicalTerm(grounded: Double, variable: Seq[Double], constant: Seq[Double])
    @term case class LogicalAtom(relation: Seq[Double], args: Seq[LogicalTerm])

    val init: STerm[Stack[LogicalAtom]] = Constant(Stack(Seq.empty, Seq.empty))

    println(push(Stack(Seq(1.0), Seq(1.0)))(1.0, 0.0, 2.0))

    //val evaluator = DefaultEval + StackEval

    val evaluator = DiffEval + BaseEval

  }
}

object DiffEval extends Evaluator {
  def partial[T](bindings: Bindings, backoff:Evaluator): PartialFunction[STerm[T], Or[T, Every[ErrorMsg]]] = {
    case Push(stack,push,pop,value) => for (vs <- backoff.eval(bindings)(stack)) yield vs //todo: implement this
  }
}

