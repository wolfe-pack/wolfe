package ml.wolfe.macros

import scala.reflect.macros.Context
import scala.reflect.api.Universe
import scala.collection.mutable

/**
 * @author Sebastian Riedel
 */
trait TransformHelper[C <: Context] extends Transformers[C] {
  this: MacroHelper[C] =>

  import context.universe._




  def replaceMethods(tree: Tree, defDefs: Map[Symbol, DefDef]) = {
    val transformer = new ReplaceMethodsWithFunctions(defDefs)
    transformer transform tree
  }

  def replaceVals(tree: Tree, defs: Map[Symbol, Tree]) = {
    transform(tree, {
      case i: Ident => defs.getOrElse(i.symbol, i)
      case t => t
    })
  }

  val dotDistributor = new DotDistributor

  def distributeDots(tree: Tree) = dotDistributor transform tree

//  def distinctTrees(trees: List[Tree], result: List[Tree] = Nil): List[Tree] = trees match {
//    case Nil => result
//    case head :: tail =>
//      val distinct = if (result.exists(_.equalsStructure(head))) result else head :: result
//      distinctTrees(tail, distinct)
//  }


  class ReplaceMethodsWithFunctions(defDefs: Map[Symbol, DefDef]) extends Transformer {
    def getDef(f: Tree) = f match {
      case TypeApply(templateFun, _) => defDefs.get(templateFun.symbol)
      case _ => defDefs.get(f.symbol)
    }

    def createFunction(defArgs: List[List[ValDef]], rhs: Tree): Function = defArgs match {
      case Nil => Function(Nil, rhs)
      case headArgs :: Nil => Function(headArgs, rhs)
      case headArgs :: tail => Function(headArgs, createFunction(tail, rhs))
    }

    override def transform(tree: Tree): Tree = tree match {
      case TypeApply(f@Ident(_), _) => getDef(f) match {
        case Some(DefDef(_, _, _, defArgs, _, rhs)) if defArgs != Nil => createFunction(defArgs, transform(rhs))
        case _ => super.transform(tree)
      }
      case f@Ident(_) => getDef(f) match {
        case Some(DefDef(_, _, _, defArgs, _, rhs)) if defArgs != Nil => createFunction(defArgs, transform(rhs))
        case _ => super.transform(tree)
      }
      case _ => super.transform(tree)
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

    override def transform(tree: Tree): Tree = {
      val transformed = tree match {
        case DotProduct(QuantifiedSum(qdom, qpred, Function(args, body)), arg2) => q"sum($qdom)($qpred)((..$args) => $body.dot($arg2))"
        case DotProduct(FlatSum(args1), FlatSum(args2)) => applyRecursively(distributeDots(args1, args2))
        case DotProduct(FlatSum(args1), arg2) => applyRecursively(distributeDots(args1, List(arg2)))
        case DotProduct(arg1, FlatSum(args2)) => applyRecursively(distributeDots(List(arg1), args2))
        case other => other
      }
      super.transform(transformed)
    }

  }

  trait ApplyBinaryOperator {
    def unapply(tree: Tree): Option[(Tree, Tree)]
    def apply(arg1: Tree, arg2: Tree): Tree
  }

  class ApplyOperatorWithName(name: String) extends ApplyBinaryOperator {
    def unapply(tree: Tree) = tree match {
      //      case Apply(s@Select(arg1, opName), List(arg2))
      //        if s.symbol.owner == definitions.DoubleClass && opName.encoded == name => Some(arg1, arg2)
      case Apply(s@Select(arg1, opName), List(arg2)) if opName.encoded == name => Some(arg1, arg2)
      case _ => None
    }
    def apply(arg1: Tree, arg2: Tree): Tree = ???
  }

  object ApplyMinus extends ApplyOperatorWithName("$minus")
  object ApplyPlus extends ApplyOperatorWithName("$plus")
  object ApplyTimes extends ApplyOperatorWithName("$times")


  object DotProduct {
    def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
      //todo: make this type-safe
      case q"$arg1.dot($arg2)" => Some(arg1, arg2)
      //      case Apply(Select(Apply(_, List(arg1)), _), List(arg2)) => Some(arg1, arg2)
      case _ => None
    }
  }


  class Flattened(operator: ApplyBinaryOperator) {
    val Match = this
    def unapply(tree: Tree): Option[List[Tree]] = tree match {
      case operator(Match(args1), Match(args2)) => Some(args1 ::: args2)
      case operator(arg1, Match(args2)) => Some(arg1 :: args2)
      case operator(Match(args1), arg2) => Some(arg2 :: args1)
      case operator(arg1, arg2) => Some(List(arg1, arg2))
      case _ => None
    }

  }

  object FlatSum extends Flattened(ApplyPlus)
  object FlatProduct extends Flattened(ApplyTimes)
  object QuantifiedSum {
    def unapply(tree: Tree): Option[(Tree, Tree, Tree)] = tree match {
      case q"sum[..${_}]($qdom)($qpred)($qobj)" => Some(qdom, qpred, qobj)
      case q"sum[..${_}]($qdom)($qpred)($qobj)(${_})" => Some(qdom, qpred, qobj)
      case _ => None
    }
  }

}


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
      val (newExpr, newStats) = stats.foldRight(expr -> List.empty[Tree]) {
        (stat, result) => stat match {
          //todo: we also need to replace vals in other statements in the list
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