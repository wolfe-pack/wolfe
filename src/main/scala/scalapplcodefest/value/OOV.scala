package scalapplcodefest.value

/**
 * An element representing an out-of-vocabulary item. While OOVs can have values, this value is just used
 * for introspection. In terms of object identity, every OOV instance is equal to every other OOV instance,
 * independent of the value of the OOV.
 * @param value the value of the OOV that can be used for introspection.
 *
 * @author Sebastian Riedel
 */
class OOV[T](val value: T) {
  override def equals(p1: scala.Any) = p1 match {
    case o: OOV[_] => true
    case _ => false
  }
  override def hashCode() = getClass.hashCode()
}
