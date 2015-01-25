package ml.wolfe.term

import ml.wolfe.fg20._

/**
 * @author riedel
 */
trait Term[T <: Domain[_]] {
  val domain: T
  def vars: Clique
  def evaluator(): Evaluator

  def apply(state: State): domain.Value = {
    val eval = evaluator()
    val output = domain.createSetting()
    val input = vars.createPartialSetting(state)
    eval.eval(input, output)
    domain.toValue(output)
  }

}

trait Evaluator {
  def eval(input: Setting, output: Setting)
}

trait Fun[A <: Domain[_], B <: Domain[_]] {
  val from: A
  val to  : B
  def apply(arg: from.Value): to.Value = {
    val input = from.createPartialSetting(from.observation(arg))
    val output = to.createSetting()
    applier().apply(input,output)
    to.toValue(output)
  }
  def applier(): Applier
}

trait Applier {
  def apply(args: Setting, result: Setting)
}

class Parameter[D <: Domain[_]](val domain:D)

trait LambdaAbstraction[A <: Domain[_], B <: Domain[_]] extends Fun[A,B] {
  def body:Term[B]
  val param:Parameter[A]
  val to = body.domain
  val from = param.domain
  //assert that free parameters of body are equal to parameter specified
  def applier() = new Applier {
    val bodyEval = body.evaluator()
    def apply(args: Setting, result: Setting) = {
      bodyEval.eval(args,result)
    }
  }
}

trait Potential[A <: Domain[_]] extends Fun[A, AtomicDomain.Cont] {
  val to = AtomicDomain.cont("result")
}


trait FunApp[A <: Domain[_], B <: Domain[_]] extends Term[B] {
  def fun: Fun[A, B]
  def arg: Term[A]

  def vars = arg.vars

  def evaluator() = new Evaluator {

    val argEval   = arg.evaluator()
    val applier   = fun.applier()
    val argOutput = arg.domain.createSetting()

    def eval(input: Setting, output: Setting) = {
      argEval.eval(input, argOutput)
      applier.apply(argOutput, output)
    }
  }

}

abstract class Max[MaxOver <: Domain[_],Params <: Domain[_]](val fun:Potential2[MaxOver,Params]) extends Potential[Params] {
  val from:Params = fun.from.arg2
}

class Sum(val from:DoubleSeqDom) extends Potential[DoubleSeqDom] {

  def applier() = new Applier {
    def apply(args: Setting, result: Setting) = {
      var tmp = 0.0
      for (i <- 0 until args.cont.length) tmp += args.cont(i)
      result.cont(0) = tmp
    }
  }
}



abstract class Tuple2Domain[T1,T2,D1 <: Domain[T1],D2 <: Domain[T2]] extends Domain[(T1,T2)] {
  def arg1:D1
  def arg2:D2
}


//trait Term[S <: SearchSpace[_]] {
//  val range: S
//
//  def vars: Clique
//
//  def evaluator(): Evaluator
//
//  def apply(state: State): range.Value  = {
//    val input = vars.createPartialSetting(state)
//    val output = range.createSetting()
//    evaluator().eval(input, output)
//    range.toValue(output)
//  }
//
//  def createOutputSetting() = range.createSetting()
//}
//
//trait Differentiable extends Potential
//trait SupportsMarginalization extends Potential
//trait SupportsMax extends Potential
//
//trait Evaluator {
//  def eval(input: Setting, output: Setting)
//}

//trait Composed[T, S <: SearchSpace[T]] extends Term[_] {
//  def children: IndexedSeq[Term[_]]
//
//  def composer(): Composer
//
//  def evaluator() = new Evaluator {
//    val inputs = children.map(_.vars.createSetting()).toArray
//    val outputs = children.map(_.createOutputSetting()).toArray
//    val evaluators = children.map(_.evaluator())
//    val mappings = children.map(c => CliqueMapping(vars, c.vars)).toArray
//
//    val compo = composer()
//
//    def eval(input: Setting, output: Setting) = {
//      for (i <- 0 until inputs.length) {
//        mappings(i).copyForward(input, inputs(i))
//        evaluators(i).eval(inputs(i), outputs(i))
//      }
//      compo.compose(outputs, output)
//    }
//  }
//
//}

//trait Neural[T, S <: SearchSpace[T]] extends Composed[T, S] {
//  def children: IndexedSeq[Neural[_, _]]
//
//  def propagator(): BackPropagator
//}

//trait Chain[T,S<:SearchSpace[T]] extends Term[_] {
//  def first:Term[_]
//  def second:Term[_]
//
//  def range = second.range
//  def vars = second.vars
//
//  def evaluator() = new Evaluator {
//    val firstEval = first.evaluator()
//    val secondEval = second.evaluator()
//    def eval(input: Setting, output: Setting) = {
//
//    }
//  }
//
//}

//abstract class NeuralLoss(val loss:Differentiable, val neural:Neural[_,_]) extends Differentiable {
//
//}

trait BackPropagator {
  def propagate(outputError: Setting, inputActivation: Setting, inputError: Setting)
}

trait Composer {
  def compose(inputs: Array[Setting], output: Setting)
}

//trait SumTerm extends ComposedPotential {
//  def composer() = new Composer {
//    def compose(inputs: Array[Setting], output: Setting) = {
//      output.cont(0) = 0.0
//      for (i <- 0 until inputs.length) output.cont(0) += inputs(i).cont(0)
//    }
//  }
//}

//abstract class LogZ(val domain: Clique, val model: SupportsMarginalization) extends Differentiable {
//  //to calculate LogZ value and gradient the model needs to support marginalization
//}
//
//abstract class Max(val domain:Clique, val model:SupportsMax) extends Differentiable {
//
//}

//abstract class ParserLoss(val scores:Neural[Map[(Int,Int),Double],_]) extends Differentiable {
//  //inner = LogZ(treeDomain, model(scores))
//  //LogZ(
//  //make sure inners' free variables are connected to
//}
//parserLoss = logZ(model(nnScores(parameters)))
//
//
//trait Fun[A<:SearchSpace[_], B<:SearchSpace[_]] {
//  val domain:A
//  val range:B
//
//  def apply(a:domain.Value):range.Value
//
//}
//
////trait FunApp[A <: SearchSpace[_], B <: SearchSpace[_], C <: FunApp] extends Fun[]
//
//trait Compose[A <: SearchSpace[_], B <: SearchSpace[_], C <: SearchSpace[_]] extends Fun[A,C] {
//  def first:Fun[A,B]
//  def second:Fun[B,C]
//}
//
//
