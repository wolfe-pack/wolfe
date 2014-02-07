package scalapplcodefest.macros

import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.collection.mutable
import scalapplcodefest.MPGraph
import scalapplcodefest.MPGraph.Factor

//import scalapplcodefest.Wolfe._

/**
 * @author Sebastian Riedel
 */
object OptimizedWolfe extends WolfeAPI {

  override def argmax[T](data: Iterable[T])
                        (where: (T) => Boolean)
                        (obj: (T) => Double) = macro implArgmax[T]

  def all[A, B](mapper: A => B)(implicit dom: Iterable[A]): Iterable[B] = macro implAll[A, B]

  def implAll[A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context)(mapper: c.Expr[A => B])(dom: c.Expr[Iterable[A]]) = {
    import c.universe._
    DomainExpansions.register(c.Expr(c.macroApplication), mapper, dom)
    reify(dom.splice map mapper.splice)
  }

  def implArgmax[T: c.WeakTypeTag](c: Context)
                                  (data: c.Expr[Iterable[T]])
                                  (where: c.Expr[T => Boolean])
                                  (obj: c.Expr[T => Double]) = {

    import c.universe._

    val helper = new MacroHelper[c.type](c)
    import helper._

    //information from the enclosing context
    val metaData = MetaStructuredGraph(data.tree, where.tree, obj.tree)
    val graphName = newTermName(c.fresh("graph"))

    val result = q"""
      import scalapplcodefest._
      import scalapplcodefest.MPGraph._

      ${metaData.classDef}
      val $graphName = new ${metaData.graphClassName}(null)
      MaxProduct($graphName.${metaData.mpGraphName},1)
      $graphName.${metaData.structureName}.setToArgmax()
      $graphName.${metaData.structureName}.value()
    """

    c.Expr[T](result)

  }
}

object DomainExpansions {

  import scala.reflect.internal.util.SourceFile

  case class DomainExpansion(term: Context#Expr[Any], constructor: Context#Expr[Any], domain: Context#Expr[Any])

  case class Location(start: Int, source: SourceFile)

  val byExpression = new mutable.HashMap[Context#Expr[Any], DomainExpansion]()
  val byPosition   = new mutable.HashMap[Location, DomainExpansion]()

  def register(term: Context#Expr[Any], constructor: Context#Expr[Any], domain: Context#Expr[Any]) = {
    val expansion = DomainExpansion(term, constructor, domain)
    byExpression(constructor) = expansion
    byPosition(Location(term.tree.pos.startOrPoint, term.tree.pos.source)) = expansion
  }

  def findByPosition(start: Int, source: SourceFile) = byPosition.get(Location(start, source))

}

class MacroHelper[C <: Context](val context: C) extends TransformHelper[C] with StructureHelper[C] {
}

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
        case Some(DefDef(_, _, _, defArgs, _, rhs)) => createFunction(defArgs, transform(rhs))
        case _ => super.transform(tree)
      }
      case f@Ident(_) => getDef(f) match {
        case Some(DefDef(_, _, _, defArgs, _, rhs)) => createFunction(defArgs, transform(rhs))
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


