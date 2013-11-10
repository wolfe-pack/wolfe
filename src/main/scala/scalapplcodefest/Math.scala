package scalapplcodefest

/**
 * @author Sebastian Riedel
 */
object Math {
  object IntAdd extends PartialFunction[(Int,Int),Int] {
    def apply(v1: (Int, Int)) = v1._1 + v1._2
    def isDefinedAt(x: (Int, Int)) = true
  }
}
