package ml.wolfe.macros

import scala.language.existentials
import scala.language.experimental.macros
import scala.reflect.macros.Context


/**
 * An inliner takes a tree, inlines functions to anonymous functions and performs beta reduction (i.e. replaces
 * function calls of anonymous functions with the right hand side of the anonymous function where parameters are
 * replaced with the function application arguments.
 */
trait CodeRepository[C <: Context] extends HasContext[C] with Transformers[C] with SymbolRepository[C] {

  import context.universe._

  //find inlineable definitions
  lazy val definitions = context.enclosingUnit.body.filter(_.symbol != NoSymbol).collect({
    case d: DefDef => d.symbol -> d
    //    case v: ValDef if !v.symbol.isParameter =>
    //      v.symbol -> DefDef(v.mods, v.name, Nil, Nil, v.tpt, v.rhs)
  }).toMap

  def get(symbol: Symbol) = {
    if (symbol == NoSymbol) None else definitions.get(symbol) match {
      case Some(d) => Some(d)
      case None =>
        val owner = symbol.owner
        owner.annotations.find(_.tpe.typeSymbol == wolfeSymbols.inlinable) match {
          case Some(inlinable) =>
            val Literal(Constant(TypeRef(_, sym, _))) = inlinable.scalaArgs.head
            val loader = getClass.getClassLoader
            val loaded = loader.loadClass(sym.fullName)
            val instance = loaded.newInstance()
            val shortName = symbol.name.encoded
            val methods = loaded.getDeclaredMethods
            val method = methods.find(_.getName == shortName).get
            val params = symbol.asMethod.paramss.map(_.map(p => {
              val vd = ValDef(NoMods, p.name.toTermName, TypeTree(p.typeSignature), EmptyTree)
              vd.symbol = p
              vd
            }))
            val args = params.flatMap(_.map(p => {
              val tree = Ident(p.symbol)
              val expr = context.Expr[Any](tree)
              expr
            }))
            val withContext = context :: args
            val result = method.invoke(instance, withContext: _*).asInstanceOf[context.Expr[Any]]
            val returnType = TypeTree(symbol.asMethod.returnType)
            Some(DefDef(NoMods, symbol.name, Nil, params, returnType, result.tree))
//            None
          case None => None
        }


      //        None
    }
  }

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
    val reduced2 = betaReduce(reduced) //todo: there are cases where after reduction there is still room for reduction. Understand why!
    if (replaced == tree) None else Some(reduced2)
  }

  def inlineN(t: Int, tree: Tree): Option[Tree] = t match {
    case 1 => inlineOnce(tree)
    case n => inlineOnce(tree).flatMap(i => inlineN(n - 1, i))
  }

  def inlineFull(tree:Tree):Tree = {
    inlineOnce(tree) match {
      case Some(inlined) => inlineFull(inlined)
      case None => tree
    }
  }


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


    def transformIfFunction(tree: Tree) = getDef(tree) match {
      case Some(DefDef(_, _, _, defArgs, _, rhs)) => defArgs match {
        case Nil => recurse(context.typeCheck(rhs))
        case _ => createFunction(defArgs, recurse(context.typeCheck(rhs)))
      }
      case _ => super.transform(tree)
    }

    override def transform(tree: Tree): Tree = tree match {
      case TypeApply(f@Ident(_), _) => transformIfFunction(f)
      //      case TypeApply(s:Select, _) => transformIfFunction(s)
      case s: Select => transformIfFunction(s)
      case f@Ident(_) => transformIfFunction(f)
      case _ => super.transform(tree)
    }
  }

}

object CodeRepository {

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

    val repo = new ContextHelper[c.type](c) with CodeRepository[c.type] //{val context:c.type = c}
    val evalTimes = c.eval(c.Expr[Int](c.resetAllAttrs(times.tree)))
    val result = for (inlined <- repo.inlineN(evalTimes, t.tree)) yield inlined.toString()
    c.literal(result.toString)
  }
}
