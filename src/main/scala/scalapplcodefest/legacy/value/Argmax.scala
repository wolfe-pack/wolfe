package scalapplcodefest.legacy.value


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

case object MaxValue extends Fun[Fun[Any,Double],Double] {
  def apply(f: Fun[Any, Double]) = f.funDom.view.map(f).max
  def funCandidateDom = new AllOfType[Fun[Any, Double]]
  def funRange = Doubles
  def isDefinedAt(x: Fun[Any, Double]) = true
}

case object Argmin extends Fun[Fun[Any,Double],Any] {
  def apply(f: Fun[Any, Double]) = f.funDom.minBy(f(_))
  def funCandidateDom = new AllOfType[Fun[Any, Double]]
  def funRange = new AllOfType[Any]
  def isDefinedAt(x: Fun[Any, Double]) = true
}

case object MinValue extends Fun[Fun[Any,Double],Double] {
  def apply(f: Fun[Any, Double]) = f.funDom.view.map(f).min
  def funCandidateDom = new AllOfType[Fun[Any, Double]]
  def funRange = Doubles
  def isDefinedAt(x: Fun[Any, Double]) = true
}


