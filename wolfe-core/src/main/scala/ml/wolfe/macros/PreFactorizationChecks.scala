package ml.wolfe.macros

import scala.language.experimental.macros
import scala.reflect.internal.Flags
import scala.reflect.macros.Context


/**
 * @author Sebastian Riedel
 */
trait PreFactorizationChecks[C <: Context] extends PatternRepository[C] with BasicPreFactorizationCheckers[C] {

  case class PreFactorizationError(position: context.Position, msg: String, suggestion: Option[String] = None)

  trait Checker {
    def check(trees: BuilderTrees): List[PreFactorizationError]
  }

  val checkers: List[Checker] = Nil //List(ValDefChecker)


  def check(trees: BuilderTrees): List[PreFactorizationError] = {
    checkers.flatMap(_.check(trees))
  }

  def outputErrorsAndAbortIfNecessary(errors: List[PreFactorizationError]): Unit = {
    if (errors.nonEmpty) {
      for (error <- errors) {
        context.warning(error.position, error.msg)
      }
      context.abort(errors.last.position,
        "Encountered wolfe specific errors when compiling operator")
    }
  }
}

object PreFactorizationChecks {

  type Error = (Int, String)
  def check[T](dom: Iterable[T], obj: T => Double): List[Error] = macro checkImp[T]

  def checkImp[T](c: Context)(dom: c.Expr[Iterable[T]], obj: c.Expr[T => Double]): c.Expr[List[Error]] = {
    val helper = new ContextHelper[c.type](c) with PreFactorizationChecks[c.type]
    val trees = helper.builderTrees(dom.tree, obj.tree)
    val errors = helper.check(trees)
    import c.universe._  //needed for quasiquotes, DONT REMOVE
    val convertedErrors = errors.map(e => q"(${e.position.point},${e.msg})")
    val convertedList:Tree = q"List(..$convertedErrors)"
    c.Expr[List[Error]](convertedList)
  }
}

trait BasicPreFactorizationCheckers[C <: Context] {
  this: PreFactorizationChecks[C] =>

  import context.universe._

  object ValDefChecker extends Checker {

    def checkTree(tree:Tree):List[PreFactorizationError] = {
      val collected = tree.collect({
        case vd:ValDef if !vd.mods.hasFlag(Flag.PARAM) && !vd.mods.hasFlag(Flags.SYNTHETIC.toLong.asInstanceOf[FlagSet]) =>
          List(PreFactorizationError(vd.pos, s"""Wolfe expects ${vd.name.toString} to be a "def" instead of a "val""""))
        case id:Ident =>
          val definition = get(id.symbol)
          definition.map(dd => checkTree(dd.rhs)).getOrElse(Nil)
      })
      collected.flatten
    }

    def check(trees: BuilderTrees) = {
      checkTree(trees.of)
    }
  }

}

