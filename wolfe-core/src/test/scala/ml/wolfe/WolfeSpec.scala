package ml.wolfe

import org.scalactic.Equality
import org.scalatest.{Matchers, WordSpec}

/**
 * Default Specs for Wolfe.
 *
 * @author Sebastian Riedel
 */
trait WolfeSpec extends WordSpec with Matchers {

  val eps = 0.0001

  implicit val factorieVectorEq = new Equality[FactorieVector] {
    def areEqual(a: FactorieVector, b: Any) = b match {
      case v: FactorieVector =>
        a.activeDomain.forall(i => math.abs(a(i) - v(i)) < eps) &&
        v.activeDomain.forall(i => math.abs(a(i) - v(i)) < eps)
      case _ => false
    }
  }

  implicit val factorieMatrixEq = new Equality[FactorieMatrix] {
    def areEqual(a: FactorieMatrix, b: Any) = b match {
      case v: FactorieMatrix =>
        a.activeDomain.forall(i => math.abs(a(i) - v(i)) < eps) &&
          v.activeDomain.forall(i => math.abs(a(i) - v(i)) < eps)
      case _ => false
    }
  }



  //  implicit val stateEq = new Equality[State] {
//    def areEqual(a: State, b: Any) = b match {
//      case that: State => a.domain == that.domain && a.domain.forall(v => {
//        (a(v), that(v)) match {
//          case (DiscDistribution(_, m1), DiscDistribution(_, m2)) =>
//            m1.keys.forall(k1 => math.abs(m1(k1) - m2(k1)) < 0.0001)
//          case (x1, x2) => x1 == x2
//        }
//      })
//      case _ => false
//    }
//  }


}
