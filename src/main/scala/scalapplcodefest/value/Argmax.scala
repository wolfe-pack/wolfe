package scalapplcodefest.value


/**
 * Returns the argument that maximize a function.
 * @author Sebastian Riedel
 */
case object Argmax extends Fun[Fun[Any,Double],Any] {
  def apply(f: Fun[Any, Double]) = f.funDom.maxBy(f(_))
  def funCandidateDom = new AllOfType[Fun[Any, Double]]
  def funRange = new AllOfType[Any]
  def isDefinedAt(x: Fun[Any, Double]) = true
}


