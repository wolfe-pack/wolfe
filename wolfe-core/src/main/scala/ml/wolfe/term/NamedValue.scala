package ml.wolfe.term

import scala.reflect.macros.blackbox
import scala.language.implicitConversions
import scala.language.experimental.macros


/**
 * @author riedel
 */
class NamedValue[T](val name:String, val value:T)

object NamedValue extends NamedValueImplicits {
  def namedMacro[T : c.WeakTypeTag](c:blackbox.Context)(value : c.Expr[T]):c.Expr[NamedValue[T]] = {
    import c.universe._
    val code = showCode(value.tree)
    val name = Constant(code)
    val result = q"""
      new ml.wolfe.term.NamedValue($name,${value.tree})
    """
    c.Expr[NamedValue[T]](result)
  }
  //  def logMacro[T <: Term[Dom] : c.WeakTypeTag](c:blackbox.Context)(term: c.Expr[T]):c.Expr[]
}

trait NamedValueImplicits {
  implicit def toNamedValue[T](value:T):NamedValue[T] =  macro NamedValue.namedMacro[T]

}