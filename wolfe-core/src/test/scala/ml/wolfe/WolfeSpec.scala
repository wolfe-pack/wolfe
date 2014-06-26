package ml.wolfe

import org.scalatest.{Matchers, WordSpec}
import org.scalautils.Equality

/**
 * Default Specs for Wolfe.
 *
 * @author Sebastian Riedel
 */
trait WolfeSpec extends WordSpec with Matchers {
  import Wolfe._

  implicit val vectorEq = new Equality[Vector] {
    def areEqual(a: Wolfe.Vector, b: Any) = b match {
      case v:Wolfe.Vector => a.keySet == v.keySet && a.keySet.forall(k => math.abs(a(k) - v(k)) < 0.0001)
      case _ => false
    }
  }

}
