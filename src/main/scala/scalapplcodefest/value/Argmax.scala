package scalapplcodefest.value

/**
 * Returns the argument that maximize a function.
 * @author Sebastian Riedel
 */
class Argmax[T] extends Fun[Fun[T,Double],T] {
  def apply(f: Fun[T, Double]) = f.funDom.maxBy(f(_))
  def funCandidateDom = new AllOfType[Fun[T, Double]]
  def funRange = new AllOfType[T]
  def isDefinedAt(x: Fun[T, Double]) = true
}
