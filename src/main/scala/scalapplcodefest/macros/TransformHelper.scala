package scalapplcodefest.macros

import scala.reflect.macros.Context

/**
 * @author Sebastian Riedel
 */
trait TransformHelper[C <: Context] {
  this: MacroHelper[C] =>

  import context.universe._

  def transform(tree: Tree, pf: PartialFunction[Tree, Tree]): context.Tree = new TransformWithPartialFunction(pf).transform(tree)

  class TransformWithPartialFunction(pf: PartialFunction[Tree, Tree]) extends Transformer {
    override def transform(tree: Tree) = {
      val transformed = super.transform(tree)
      if (pf.isDefinedAt(transformed)) pf(transformed) else transformed
    }
  }

  def replaceMethods(tree: Tree, defDefs: Map[Symbol, DefDef]) = {
    val transformer = new ReplaceMethodsWithFunctions(defDefs)
    transformer transform tree
  }

  def replaceVals(tree:Tree, defs:Map[Symbol,Tree]) = {
    transform(tree, {
      case i:Ident => defs.getOrElse(i.symbol, i)
      case t => t
    })
  }


  val betaReducer     = new BetaReducer
  val blockSimplifier = new BlockSimplifier

  def betaReduce(tree: Tree) = betaReducer transform tree
  def simplifyBlocks(tree: Tree) = blockSimplifier transform tree

  def distinctTrees(trees: List[Tree], result: List[Tree] = Nil): List[Tree] = trees match {
    case Nil => result
    case head :: tail =>
      val distinct = if (result.exists(_.equalsStructure(head))) result else head :: result
      distinctTrees(tail, distinct)
  }

  class BlockSimplifier extends Transformer {
    override def transform(tree: Tree) = tree match {
      case Block(Nil, expr) => super.transform(expr)
      case _ => super.transform(tree)
    }
  }

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

  class Substituter(binding: Map[Symbol, Tree]) extends Transformer {
    override def transform(tree: Tree) = tree match {
      case i: Ident => binding.get(i.symbol) match {
        case Some(value) => value
        case _ => super.transform(tree)
      }
      case _ => super.transform(tree)
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


  trait ApplyBinaryOperator {
    def unapply(tree: Tree): Option[(Tree, Tree)]
  }

  class ApplyDoubleOperator(name: String) extends ApplyBinaryOperator {
    def unapply(tree: Tree) = tree match {
      //      case Apply(s@Select(arg1, opName), List(arg2))
      //        if s.symbol.owner == definitions.DoubleClass && opName.encoded == name => Some(arg1, arg2)
      case Apply(s@Select(arg1, opName), List(arg2)) if opName.encoded == name => Some(arg1, arg2)
      case _ => None
    }
  }

  object ApplyDoubleMinus extends ApplyDoubleOperator("$minus")
  object ApplyDoublePlus extends ApplyDoubleOperator("$plus")
  object ApplyDoubleTimes extends ApplyDoubleOperator("$times")


  object DotProduct {
    def unapply(tree: Tree) = tree match {
        //todo: this doesn't check if there is an actual dot product!
      case Apply(Select(Apply(_, List(arg1)), _), List(arg2)) => Some(arg1, arg2)
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

  object FlatDoubleSum extends Flattened(ApplyDoublePlus)
  object FlatDoubleProduct extends Flattened(ApplyDoubleTimes)

}
