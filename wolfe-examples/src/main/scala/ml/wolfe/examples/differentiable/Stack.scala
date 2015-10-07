package ml.wolfe.examples.differentiable

/**
 * @author riedel
 */
//@term case class Stack[C](store: Seq[C], strength: Seq[Double])
//
//object StackOps {
//
//  import Wolfe._
//
//  case class Push[C](stack: STerm[Stack[C]], push: STerm[Double], pop: STerm[Double], value: STerm[C]) extends STerm[Stack[C]]
//
//  def push[C](stack: STerm[Stack[C]])(push: STerm[Double], pop: STerm[Double], value: STerm[C]) = {
//    val store = stack.store :+ value
//    val zeroes = fill(stack.strength.length)(0.0)
//    val pops = fill(stack.strength.length)(pop)
//    val accumulated = RangeTerm(0, stack.strength.length).map {
//      i => stack.strength.slice(i + 1, stack.strength.length - 1).sum
//    }
//    val updatedStrength = max(zeroes, stack.strength - max(zeroes, pops - accumulated))
//    val strength = updatedStrength :+ push
//    Stack.Term(store, strength)
//  }
//
//  def read[C](stack: STerm[Stack[C]]) = {
//
//  }
//
//  def head[C](stack: STerm[Stack[C]]): STerm[C] = stack.store(0)
//
//  def main(args: Array[String]) {
//
//    @term case class LogicalTerm(grounded: Double, variable: Seq[Double], constant: Seq[Double])
//    @term case class LogicalAtom(relation: Seq[Double], args: Seq[LogicalTerm])
//
//    val init: STerm[Stack[LogicalAtom]] = Constant(Stack(Seq.empty, Seq.empty))
//
//    println(push(Stack(Seq(1.0), Seq(1.0)))(1.0, 0.0, 2.0))
//
//    //val evaluator = DefaultEval + StackEval
//
//    val evaluator = DiffEval + BaseEval
//
//  }
//}

//object DiffEval extends Evaluator {
//  def partial[T](bindings: Bindings, backoff: Evaluator): PartialFunction[STerm[T], Or[T, Every[ErrorMsg]]] = {
//    case Push(stack, push, pop, value) => for (vs <- backoff.eval(bindings)(stack)) yield vs //todo: implement this
//  }
//}

object DeterministicTheoremProver {

  //using foldLeft
  sealed trait LogTerm

  case class LogVar(name: String) extends LogTerm

  case class LogConst(name: String) extends LogTerm

  case class LogAtom(pred: String, args: List[LogTerm])

  case class Rule(head: LogAtom, body: List[LogAtom] = Nil)

  case class Substitution(bindings: Map[LogVar, LogTerm] = Map.empty, failure: Boolean = false)

  case class KB(rules: List[Rule])

  /**
   * Based on N&R, AI, chapter 10 and Slide 25 here
   * http://www.ra.cs.uni-tuebingen.de/lehre/uebungen/ws13/AI/skript/AI_09_Inference_in_First_Order_Logic.pdf
   */
  def unify(x: Any, y: Any, previous: Substitution = Substitution(Map.empty, false)): Substitution = (x, y) match {
    case _ if previous.failure => previous
    case _ if x == y => previous
    case (v: LogVar, _) => unifyVar(v, y, previous)
    case (_, v: LogVar) => unifyVar(v, x, previous)
    case (LogAtom(p1, args1), LogAtom(p2, args2)) if p1 == p2 => unify(args1, args2, previous)
    case (h1 :: t1, h2 :: t2) => unify(t1, t2, unify(h1, h2, previous))
    case _ => Substitution(Map.empty, true)
  }

  def unifyVar(v: LogVar, x: Any, previous: Substitution): Substitution =
    (previous.bindings.get(v), x) match {
      case (Some(value), _) => unify(value, x, previous)
      case (None, l: LogVar) if previous.bindings.isDefinedAt(l) => unify(v, previous.bindings(l), previous)
      //todo: check if contains
      case _ => previous.copy(bindings = previous.bindings + (v -> x.asInstanceOf[LogTerm]))
    }

  def compose(theta1: Substitution, theta2: Substitution) = {
    val joined = for ((key, value) <- theta1.bindings) yield value match {
      case l: LogVar if theta2.bindings.isDefinedAt(l) => key -> theta2.bindings(l)
      case _ => key -> value
    }
    val onlyInTheta2 = theta2.bindings.filterNot(b => theta1.bindings.isDefinedAt(b._1))
    Substitution(joined ++ onlyInTheta2, theta1.failure || theta2.failure)
  }

  def subst(atom: LogAtom, theta: Substitution): LogAtom = atom.copy(args = atom.args.map {
    case l: LogVar if theta.bindings.isDefinedAt(l) =>
      theta.bindings(l)
    case t =>
      t
  })

  def fol_bc_or(kb: KB, goal: LogAtom, theta: Substitution = Substitution()) = {
    for (rule <- kb.rules;
         unified = unify(rule.head, goal, theta)
         if !unified.failure;
         thetaPrime <- fol_bc_and(kb, rule.body, unified)) yield thetaPrime
  }

  def fol_bc_and(kb: KB, goals: List[LogAtom], theta: Substitution): List[Substitution] = {
    if (theta.failure) Nil
    else if (goals.isEmpty) List(theta)
    else {
      val first = goals.head
      val rest = goals.tail
      for (thetaPrime <- fol_bc_or(kb, subst(first, theta), theta);
           thetaPrimePrime <- fol_bc_and(kb, rest, thetaPrime)) yield thetaPrimePrime

    }
  }

  implicit class RichPred2(name: Symbol) {
    def apply(args: String*) = LogAtom(name.name, args.map {
      case l if l.charAt(0).isUpper => LogVar(l)
      case c => LogConst(c)
    }.toList)
  }

  implicit class RichAtom(atom: LogAtom) {
    def :-(body: LogAtom*) = Rule(atom, body.toList)
  }

  implicit def atomToRule(atom: LogAtom): Rule = Rule(atom)


  def main(args: Array[String]) {
    val rules: List[Rule] = List(
      'parent("jim", "john"),
      'parent("john", "mary"),
      'grandparent("X", "Z") :-('parent("X", "Y"), 'parent("Y", "Z"))
    )

    val query = 'grandparent("X?", "Y?")

    //val result = backChainList(KB(rules), List(query))

    println(fol_bc_or(KB(rules),query))

//    println(result.mkString("\n"))


  }

  //using recursion


}
