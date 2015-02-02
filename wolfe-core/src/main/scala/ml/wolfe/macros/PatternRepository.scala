package ml.wolfe.macros

import scala.reflect.macros.blackbox
import scala.reflect.macros.blackbox.Context
import ml.wolfe.Wolfe._
import scala.Some

/**
 * @author Sebastian Riedel
 */
trait PatternRepository[C <: blackbox.Context] extends SymbolRepository[C] with CodeRepository[C] {

  import context.universe._

  class InfixApply(ops: Set[Symbol]) {
    def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
      case q"$arg1Op($arg2)" if ops(arg1Op.symbol) => arg1Op match {
        case q"$arg1.${ _ }" => Some(arg1, arg2)
        case _ => None
      }
      case _ => None
    }
  }

  class AppliedOperator(classes: Symbol => Boolean, wolfeSymbol: Symbol => Boolean, scalaSymbol: Symbol) {
    def unapply(tree: Tree): Option[BuilderTrees] = {
      tree match {
        //scala style
        case q"$op[$opType]($impArg)" if op.symbol == scalaSymbol && classes(opType.symbol) => op match {
          case q"$map[..${ _ }]($obj)(${ _ }).${ _ }" if map.symbol == scalaSymbols.map => map match {
            case q"$filter($where).map" if filter.symbol == scalaSymbols.filter =>
              val q"$dom.filter" = filter
              Some(BuilderTrees(dom, where, obj, implicitArg = impArg))
            case q"$dom.map" =>
              Some(BuilderTrees(dom, EmptyTree, obj, implicitArg = impArg))
            case _ => None
          }
          case _ => None
        }
        //maxBy signature style
        case q"$op[$opType]($obj)($impArg)" if op.symbol == scalaSymbol && classes(opType.symbol) => op match {
          case q"$filter($where).${ _ }" if filter.symbol == scalaSymbols.filter =>
            val q"$dom.filter" = filter
            Some(BuilderTrees(dom, where, obj, implicitArg = impArg))
          case dom =>
            Some(BuilderTrees(dom, EmptyTree, obj, implicitArg = impArg))
          //          case _ => None

        }
        //builder-type style
//        case q"$op[$domType,$opType]($overWhereOf)($impArg)" if wolfeSymbol(op.symbol) && classes(opType.symbol) =>
//          val trees = builderTrees(overWhereOf).copy(implicitArg = impArg)
//          Some(trees)

        //new simplified style
        case q"$op[$domType,$opType]($overWhere)($of)($impArg)" if wolfeSymbol(op.symbol) && classes(opType.symbol) =>
          val trees = builderTrees(overWhere, of).copy(implicitArg = impArg)
          Some(trees)

        //simplified style w/o implicit args
        case q"$op[$domType]($overWhere)($of)" if wolfeSymbol(op.symbol) =>
          val trees = builderTrees(overWhere, of)
          Some(trees)


        case _ => None
      }
    }

  }


  object Dot {
    def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
      case q"$arg1.dot($arg2)" =>
        Some(arg1, arg2)
      case _ => None
    }
  }

  object LogZ extends AppliedOperator(Set(scalaSymbols.doubleClass), wolfeSymbols.logZs, null)

  object DoubleMax extends AppliedOperator(Set(scalaSymbols.doubleClass), wolfeSymbols.maxes, scalaSymbols.max) {

    //    def sameFunction(f1:Tree, f2:Tree)

    def collapseCurriedFunction(tree: Tree) = tree match {
      case Function(List(arg1), Apply(f, List(arg2))) if arg1.symbol == arg2.symbol => f
      case _ => tree
    }

    def normalizeFunction(f: Tree) = collapseCurriedFunction(unwrapSingletonBlocks(f))

    def isArgMaxWithFunction(term: Tree, function: Tree): Option[BuilderTrees] = term match {
      case ArgmaxOperator(trees) if normalizeFunction(trees.of).equalsStructure(normalizeFunction(function)) =>
        Some(trees)
      case _ => inlineOnce(term) match {
        case Some(inlined) => isArgMaxWithFunction(inlined, function)
        case _ => None
      }
    }

    override def unapply(tree: Tree) = super.unapply(tree) match {
      case s: Some[_] => s
      case None => tree match {
        case Apply(f, List(arg)) => isArgMaxWithFunction(arg, f)
        case _ => None
      }
    }
  }

  object AlwaysSame {
    def unapply(pair: (Tree, Tree)) = {
      pair._1.equalsStructure(pair._2)
    }
  }

  object CaseClassCopy {

    def unapply(tree: Tree): Option[(Tree, List[Tree])] = tree match {
      //todo: caseObject should be checked to be a case class, but in some contexts the tree we get is untyped
      case Block(argDefs, q"$caseObject.copy(..$args)") => //if caseObject.tpe.typeSymbol.asClass.isCaseClass =>
        val mapping = argDefs.flatMap(_.collect { case vd: ValDef => vd.symbol -> vd.rhs }).toMap
        val mappedArgs = args.map((a: Tree) => mapping.get(a.symbol))
        if (mappedArgs.exists(_.isEmpty))
          None
        else
          Some((caseObject, mappedArgs.map(_.get)))
      case q"$caseObject.copy(..$args)" => // if caseObject.tpe.typeSymbol.asClass.isCaseClass =>
        Some((caseObject, args))
      case _ =>
        None
    }
  }

  class Flattened(operator: InfixApply) {
    val Match = this
    def unapply(tree: Tree): Option[List[Tree]] = tree match {
      case operator(Match(args1), Match(args2)) => Some(args1 ::: args2)
      case operator(arg1, Match(args2)) => Some(arg1 :: args2)
      case operator(Match(args1), arg2) => Some(arg2 :: args1)
      case operator(arg1, arg2) => Some(List(arg1, arg2))
      case _ => None
    }

  }

  object ApplyAnd extends InfixApply(Set(scalaSymbols.and))
  object ApplyDoublePlus extends InfixApply(scalaSymbols.doublePluses)
  object ApplyPlus extends InfixApply(scalaSymbols.doublePluses ++ wolfeSymbols.vectorPluses)
  object ApplyDoubleMinus extends InfixApply(scalaSymbols.doubleMinuses)
  object ApplyMinus extends InfixApply(scalaSymbols.doubleMinuses ++ wolfeSymbols.vectorMinuses)
  object DoubleSum extends AppliedOperator(Set(scalaSymbols.doubleClass), wolfeSymbols.sums, scalaSymbols.sum)
  object Sum extends AppliedOperator(Set(scalaSymbols.doubleClass, wolfeSymbols.vectorType,scalaSymbols.intClass), wolfeSymbols.sums, scalaSymbols.sum)
  object ArgmaxOperator extends AppliedOperator(Set(scalaSymbols.doubleClass), wolfeSymbols.argmaxes, scalaSymbols.maxBy)
  object MapOperator extends AppliedOperator(_ => true, wolfeSymbols.maps, scalaSymbols.map)
  object FlattenedPlus extends Flattened(ApplyPlus)


  case class BuilderTrees(over: Tree = EmptyTree, where: Tree = EmptyTree, of: Tree = EmptyTree,
                          using: Tree = EmptyTree, implicitArg: Tree = EmptyTree)

  def builderTrees(tree: Tree): BuilderTrees = tree match {
    case q"$of[${ _ }]($obj)" if of.symbol == wolfeSymbols.of =>
      val q"$owo.of" = of
      builderTrees(owo).copy(of = obj)
    case q"$st($filter)" if st.symbol == wolfeSymbols.st =>
      val q"$owo.st" = st
      builderTrees(owo).copy(where = filter)
    case q"$where($filter)" if where.symbol == wolfeSymbols.where =>
      val q"$owo.where" = where
      builderTrees(owo).copy(where = filter)
    case q"$using($mapper)" if using.symbol == wolfeSymbols.using =>
      val q"$owo.using" = using
      builderTrees(owo).copy(using = mapper)
    case q"$over($dom)" if over.symbol == wolfeSymbols.over =>
      BuilderTrees(dom)
    case _ => inlineOnce(tree) match {
      case Some(inlined) =>
        builderTrees(inlined)
      case None =>
        context.error(context.enclosingPosition, "Can't analyze over-where-of clause " + tree)
        BuilderTrees()
    }
  }

  def builderTrees(dom: Tree, obj: Tree, using: Tree = EmptyTree) = dom match {
    case q"$iterable.filter($pred)" =>
      BuilderTrees(iterable, pred, obj, using, EmptyTree)
    case q"ml.wolfe.Wolfe.RichIterable[${ _ }]($iterable).where($pred)" =>
      BuilderTrees(iterable, pred, obj, using, EmptyTree)
    case q"ml.wolfe.Wolfe.RichIterable[${ _ }]($iterable).st($pred)" =>
      BuilderTrees(iterable, pred, obj, using, EmptyTree)
    case _ => BuilderTrees(dom, EmptyTree, obj, using, EmptyTree)
  }


}
