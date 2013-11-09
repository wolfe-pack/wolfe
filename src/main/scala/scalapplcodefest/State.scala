package scalapplcodefest

/**
 * A state maps variables to values.
 *
 * @author Sebastian Riedel
 */
trait State {

  def get[T](variable:Variable[T]):Option[T]

}
