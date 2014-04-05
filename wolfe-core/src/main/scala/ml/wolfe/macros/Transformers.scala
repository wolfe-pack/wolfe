package ml.wolfe.macros

import scala.reflect.macros.Context
import scala.reflect.api.Universe
import scala.collection.mutable


/**
 * A group of general purpose transformers defined with respect to some universe.
 */
trait Transformers[C<:Context] {

  this: HasContext[C] =>

  import context.universe._

  lazy val betaReducer     = new BetaReducer
  lazy val blockSimplifier = new BlockSimplifier

  def betaReduce(tree: Tree) = betaReducer transform tree
  def unwrapSingletonBlocks(tree: Tree) = blockSimplifier transform tree

  def simplifyBlock(tree: Tree): Tree = tree match {
    case Block(stats, expr) => {
      //Note that we are going right to left. This means that val definitions *can* use previous val definitions.
      val (newExpr, newStats) = stats.foldRight(expr -> List.empty[Tree]) {
        (stat, result) => stat match {
          case v: ValDef => transform(result._1, { case i: Ident if i.symbol == v.symbol => v.rhs }) -> result._2
          case i: Import => result
          case s => (result._1, result._2 :+ s)
        }
      }
      if (newStats.isEmpty) newExpr else Block(newStats,newExpr)
    }
    case _ => tree
  }


  def transform(tree: Tree, pf: PartialFunction[Tree, Tree]): context.Tree =
    new TransformWithPartialFunction(pf).transform(tree)

  def transformAndCollect[T](tree:Tree, pf:PartialFunction[Tree,(Tree,T)]):(Tree,List[T]) = {
    val transformer = new CollectingTransformer[T](pf)
    val result = transformer.transform(tree)
    (result,transformer.collected)
  }

  class Substituter(binding: Map[Symbol, Tree]) extends Transformer {
    override def transform(tree: Tree) = tree match {
      case i: Ident => binding.get(i.symbol) match {
        case Some(value) => value
        case _ => super.transform(tree)
      }
      case _ => super.transform(tree)
    }
  }

  class TransformWithPartialFunction(pf: PartialFunction[Tree, Tree]) extends Transformer {
    override def transform(tree: Tree) = {
      val transformed = super.transform(tree)
      if (pf.isDefinedAt(transformed)) pf(transformed) else transformed
    }
  }

  class CollectingTransformer[T](pf: PartialFunction[Tree, (Tree,T)]) extends Transformer {

    var collected:List[T] = Nil
    override def transform(tree: Tree) = {
      val transformed = super.transform(tree)
      if (pf.isDefinedAt(transformed)) {
        val (toReturn,toAdd) = pf(transformed)
        collected ::= toAdd
        toReturn
      } else transformed
    }
  }


  class DotDistributor extends Transformer {

    def applyRecursively(args: List[Tree], op: TermName = newTermName("+"), result: Tree = EmptyTree): Tree = args match {
      case Nil => result
      case head :: tail =>
        val select = if (result.isEmpty) head else q"$result.$op($head)"
        applyRecursively(tail, op, select)
    }

    def distributeDots(args1: List[Tree], args2: List[Tree]): List[Tree] =
      for (a1 <- args1; a2 <- args2) yield q"$a1.dot($a2)"

//    override def transform(tree: Tree): Tree = {
//      val transformed = tree match {
//        case DotProduct(QuantifiedSum(qdom, qpred, Function(args, body)), arg2) => q"sum($qdom)($qpred)((..$args) => $body.dot($arg2))"
//        case DotProduct(FlatSum(args1), FlatSum(args2)) => applyRecursively(distributeDots(args1, args2))
//        case DotProduct(FlatSum(args1), arg2) => applyRecursively(distributeDots(args1, List(arg2)))
//        case DotProduct(arg1, FlatSum(args2)) => applyRecursively(distributeDots(List(arg1), args2))
//        case other => other
//      }
//      super.transform(transformed)
//    }

  }



  class BetaReducer extends Transformer {

    def substitute(defArgs: List[ValDef], args: List[Tree], tree: Tree): Tree = {
      val binding = (defArgs.map(_.symbol) zip args).toMap
      val substituter = new Substituter(binding)
      val result = substituter transform tree
      result
    }


    override def transform(tree: Tree): Tree = {
      val transformed = super.transform(tree)
      transformed match {
        case Apply(Function(defArgs, rhs), args) => substitute(defArgs, args, rhs)
        case Apply(Select(Function(defArgs, rhs),name), args) if name.encoded == "apply" => substitute(defArgs, args, rhs)
        case Apply(TypeApply(Function(defArgs, rhs), _),args) => substitute(defArgs, args, rhs)
        case other => other
      }
    }
  }

  class BlockSimplifier extends Transformer {
    override def transform(tree: Tree) = tree match {
      case Block(Nil, expr) => super.transform(expr)
      case _ => super.transform(tree)
    }
  }

  trait WithFunctionStack {

    private val functionStack = new mutable.Stack[Function]()

    def pushIfFunction(tree: Tree) {
      tree match {
        case f: Function => functionStack.push(f)
        case _ =>
      }
    }

    def popIfFunction(tree: Tree) {
      tree match {
        case _: Function => functionStack.pop
        case _ =>
      }
    }

    def hasFunctionArgument(tree: Tree) = {
      val symbols = tree.collect({case i: Ident => i}).map(_.name).toSet //todo: this shouldn't just be by name
      functionStack.exists(_.vparams.exists(p => symbols(p.name)))
    }
  }



}