package ml.wolfe.macros

import scala.tools.reflect.ToolBox
import scala.reflect.api.Universe
import scala.language.existentials
import scala.language.experimental.macros
import scala.reflect.macros.Context


/**
 * An inliner takes a tree, inlines functions to anonymous functions and performs beta reduction (i.e. replaces
 * function calls of anonymous functions with the right hand side of the anonymous function where parameters are
 * replaced with the function application arguments.
 * @tparam Uni
 */
trait CodeRepository[Uni <: Universe] extends InUniverse with Transformers {

  type U = Uni
  val universe: U

  import universe._

  def get(symbol: Symbol): Option[DefDef]

  /**
   * Inlines a tree once. That is, it replaces and inlines all occurrences of methods defined in the scope of the inliner
   * but won't inline symbols in the replacement.
   * @param tree the tree to do inlining in.
   * @return Some(inlined) if symbols have been replaced, None otherwise.
   */
  def inlineOnce(tree: Tree): Option[Tree] = {
    val replacer = new ReplaceMethodsWithFunctions(false)
    val replaced = replacer transform tree
    val reduced = betaReduce(replaced)
    if (reduced == tree) None else Some(reduced)
  }

  def typeCheckIfNeeded(tree: Tree): Tree

  class ReplaceMethodsWithFunctions(recursive: Boolean = true) extends Transformer {
    def getDef(f: Tree) = f match {
      case TypeApply(templateFun, _) => get(templateFun.symbol)
      case _ => get(f.symbol)
    }

    def createFunction(defArgs: List[List[ValDef]], rhs: Tree): Function = defArgs match {
      case Nil => Function(Nil, rhs)
      case headArgs :: Nil => Function(headArgs, rhs)
      case headArgs :: tail => Function(headArgs, createFunction(tail, rhs))
    }

    def recurse(tree: Tree) = if (recursive) transform(tree) else tree

    override def transform(tree: Tree): Tree = tree match {
      case TypeApply(f@Ident(_), _) => getDef(f) match {
        case Some(DefDef(_, _, _, defArgs, _, rhs)) => defArgs match {
          case Nil => recurse(typeCheckIfNeeded(rhs))
          case _ => createFunction(defArgs, recurse(typeCheckIfNeeded(rhs)))
        }
        case _ => super.transform(tree)
      }
      case f@Ident(_) => getDef(f) match {
        case Some(DefDef(_, _, _, defArgs, _, rhs)) => defArgs match {
          case Nil => recurse(typeCheckIfNeeded(rhs))
          case _ => createFunction(defArgs, recurse(typeCheckIfNeeded(rhs)))
        }
        case _ => super.transform(tree)
      }
      case _ => super.transform(tree)
    }
  }

}

trait ToolboxCodeRepository[Uni <: Universe] extends CodeRepository[Uni] {
  def addCode(code: String): universe.Tree
}

object CodeRepository {

  /**
   * Creates an inliner that uses the symbols of the provided macro context for inlining.
   * @param context the context to use symbol definition froms.
   * @tparam C context type.
   * @return Inliner that can inline expressions that use symbols of the context.
   */
  def fromContext[C <: Context](context: C): CodeRepository[context.universe.type] =
    new CodeRepository[context.universe.type] {
      val universe: context.universe.type = context.universe

      import universe._

      //find inlineable definitions
      val definitions = context.enclosingUnit.body.collect({
        case d: DefDef => d.symbol -> d
      }).toMap

      def get(symbol: universe.Symbol) = definitions.get(symbol)
      def typeCheckIfNeeded(tree: Tree) = context.typeCheck(tree)
    }

  def fromToolbox[U <: Universe](toolbox: ToolBox[U]): ToolboxCodeRepository[toolbox.u.type] = {
    new ToolboxCodeRepository[toolbox.u.type] {
      val universe: toolbox.u.type = toolbox.u

      import universe._

      var defs : Map[Symbol, DefDef] = Map.empty
      var codes: List[Tree]          = Nil

      def get(symbol: universe.Symbol) = defs.get(symbol)
      def typeCheckIfNeeded(tree: Tree) = tree
      // not needed as trees are already typed in the definition map.
      def addCode(code: String) = {
        val parsed = toolbox.parse(code)
        codes = codes :+ parsed
        val block = Block(codes, EmptyTree)
        val typed = toolbox.typeCheck(block)
        defs = typed.collect({
          case d: DefDef => d.symbol -> d //toolbox.resetAllAttrs(d).asInstanceOf[DefDef]
        }).toMap
        println(defs)
        val Block(result, _) = typed
        result.last
      }
    }
  }

  /**
   * This macro helps to test the inliner code. It returns the result of inlining an expression
   * as string that represents the result of the inline operation (including Some/None information). Ideally this should
   * be an Option[Universe#Tree] object, but due to technical reasons I haven't been able to do this yet.
   * @param t expression to inline.
   * @param times how many times should inlining be performed
   * @tparam T type of expression.
   * @return "Some(inlinedTree.toString)" if there were symbols to be inlined, "None" otherwise.
   */
  def inlineMacro[T](t: T, times: Int = 1): String = macro inlineMacroImpl[T]

  def inlineMacroImpl[T: c.WeakTypeTag](c: Context)(t: c.Expr[T], times: c.Expr[Int]): c.Expr[String] = {
    val inliner = CodeRepository.fromContext(c)
    def inline(t: Int, tree: inliner.universe.Tree): Option[inliner.universe.Tree] = t match {
      case 1 => inliner.inlineOnce(tree)
      case n => inliner.inlineOnce(tree).flatMap(i => inline(n - 1, i))
    }
    val evalTimes = c.eval(c.Expr[Int](c.resetAllAttrs(times.tree)))
    val result = for (inlined <- inline(evalTimes, t.tree)) yield inlined.toString()
    c.literal(result.toString)
  }
}

/**
 * Shared toolbox because they are expensive to create.
 */
object GlobalToolbox {

  val toolBox = scala.reflect.runtime.currentMirror.mkToolBox()

}
