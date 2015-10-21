package ml.wolfe.term

import scala.reflect.macros.blackbox
import scala.language.experimental.macros

/**
 * @author riedel
 */
trait NameProvider {
  def newName():String
}

case class FixedNameProvider(name:String) extends NameProvider {
  def newName() = name
}

trait NameProviderImplicits {
  import NameProvider._

  implicit def createValDefBasedProvider:NameProvider = macro createValDefBasedProviderImpl

}

object NameProvider {

  def createValDefBasedProviderImpl(c:blackbox.Context):c.Expr[NameProvider] = {
    import c.universe._
    val owner = c.internal.enclosingOwner
    val name = owner.name.decodedName.toString
    val asConst = Constant(name)

    val result = q"""
      new ml.wolfe.term.FixedNameProvider($asConst)
    """
    c.Expr[NameProvider](result)
  }
}
