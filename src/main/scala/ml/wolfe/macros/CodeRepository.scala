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
 */
trait CodeRepository extends HasUniverse with Transformers {

  import universe._

  def get(symbol: Symbol): Option[DefDef]

  def fresh(name: String): String

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

trait ToolboxCodeRepository extends CodeRepository {
  def addCodeString(code: String): universe.Tree
  def addCodeTree(code: universe.Tree): universe.Tree
  def addCodeTrees(codes: Seq[universe.Tree]) = for (c <- codes) addCodeTree(c)
  def pop: universe.Tree
  def eval[T]: T
}

object CodeRepository {

  /**
   * Creates an inliner that uses the symbols of the provided macro context for inlining.
   * @param context the context to use symbol definition froms.
   * @return Inliner that can inline expressions that use symbols of the context.
   */
  def fromContext(context: Context): CodeRepository {type U = context.universe.type} =
    new CodeRepository {
      val universe: context.universe.type = context.universe
      type U = context.universe.type

      import universe._

      //find inlineable definitions
      val definitions = context.enclosingUnit.body.collect({
        case d: DefDef => d.symbol -> d
        case v: ValDef => v.symbol -> DefDef(v.mods,v.name, Nil, Nil, v.tpt, v.rhs)
      }).toMap

      def get(symbol: universe.Symbol) = definitions.get(symbol)

      def fresh(name: String) = context.fresh(name)
      def typeCheckIfNeeded(tree: Tree) = context.typeCheck(tree)
    }

  def fromToolbox(toolbox: ToolBox[_ <: Universe]): ToolboxCodeRepository {type U = toolbox.u.type} = {
    new ToolboxCodeRepository {
      type U = toolbox.u.type
      val universe: toolbox.u.type = toolbox.u

      import universe._

      var defs      : Map[Symbol, DefDef] = Map.empty
      var codes     : List[Tree]          = Nil
      var typedCodes: List[Tree]          = Nil

      def get(symbol: universe.Symbol) = defs.get(symbol)

      def fresh(name: String) = name
      //todo: should be unique
      def typeCheckIfNeeded(tree: Tree) = tree

      def pop = {
        val result = typedCodes.last
        typedCodes = typedCodes.dropRight(1)
        codes = codes.dropRight(1)
        result
      }
      // not needed as trees are already typed in the definition map.

      def eval[T] = {
        val block = Block(codes.dropRight(1), codes.last)
        val reset = toolbox.resetAllAttrs(block)
        println(reset)
        val result = toolbox.eval(reset)
        result.asInstanceOf[T]
      }

      def addCodeTree(code: Tree): Tree = {
        val parsed = toolbox.resetLocalAttrs(code)
        codes = codes :+ parsed
        val block = Block(codes, EmptyTree)
        println(block)
        val typed = toolbox.typeCheck(block)
        defs = typed.collect({
          case d: DefDef => d.symbol -> d //toolbox.resetAllAttrs(d).asInstanceOf[DefDef]
        }).toMap
        //        println(defs)
        val Block(result, _) = typed
        typedCodes = result
        result.last
      }

      def addCodeString(code: String): Tree = {
        val parsed = toolbox.parse(code)
        addCodeTree(parsed)
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
    val repo = CodeRepository.fromContext(c)
    def inline(t: Int, tree: repo.universe.Tree): Option[repo.universe.Tree] = t match {
      case 1 => repo.inlineOnce(tree)
      case n => repo.inlineOnce(tree).flatMap(i => inline(n - 1, i))
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
