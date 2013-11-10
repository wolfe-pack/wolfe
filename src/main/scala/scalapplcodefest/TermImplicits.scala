package scalapplcodefest

import scala.language.implicitConversions
/**
 * @author Sebastian Riedel
 */
object TermImplicits {

  implicit def intToConstant(x: Int) = Constant(x)
  implicit def setToConstant[T](x: Set[T]) = Constant(x)


}
