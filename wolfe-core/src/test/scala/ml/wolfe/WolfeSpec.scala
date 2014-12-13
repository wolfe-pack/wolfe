package ml.wolfe

import ml.wolfe.fg20.{DiscDistribution, State}
import org.scalatest.{Matchers, WordSpec}
import org.scalautils.Equality

/**
 * Default Specs for Wolfe.
 *
 * @author Sebastian Riedel
 */
trait WolfeSpec extends WordSpec with Matchers {
  import ml.wolfe.Wolfe._

  implicit val vectorEq = new Equality[Vector] {
    def areEqual(a: Wolfe.Vector, b: Any) = b match {
      case v:Wolfe.Vector => a.keySet == v.keySet && a.keySet.forall(k => math.abs(a(k) - v(k)) < 0.0001)
      case _ => false
    }
  }

  implicit val stateEq = new Equality[State] {
    def areEqual(a: State, b: Any) = b match {
      case that:State => a.domain == that.domain && a.domain.forall(v => {
        (a(v),that(v)) match {
          case (DiscDistribution(_,m1),DiscDistribution(_,m2)) =>
            m1.keys.forall(k1 => math.abs(m1(k1) - m2(k1)) < 0.0001)
          case (x1,x2) => x1 == x2
        }
      })
      case _ => false
    }
  }


}
