package ml.wolfe.term

import ml.wolfe.term.Tensor
import org.scalactic.Equality
import org.scalatest.{Matchers, WordSpec}

/**
 * Default Specs for Wolfe.
 *
 * @author Sebastian Riedel
 */
trait WolfeSpec extends WordSpec with Matchers {

  val eps = 0.0001

  def beStringEqual(that:Any) = be(that.toString) compose( (f:Any) => f.toString)


  /*
  implicit val factorieVectorEq = new Equality[Vect] {
    def areEqual(a: Vect, b: Any) = b match {
      case v: Vect =>
        //a.dim1 == v.dim1 &&
        a.activeDomain.forall(i => math.abs(a(i) - v(i)) < eps) &&
          v.activeDomain.forall(i => math.abs(a(i) - v(i)) < eps)
      case _ => false
    }
  }

  implicit val factorieMatrixEq = new Equality[Mat] {
    def areEqual(a: Mat, b: Any) = b match {
      case v: Mat =>
        a.activeDomain.forall(i => math.abs(a(i) - v(i)) < eps) &&
          v.activeDomain.forall(i => math.abs(a(i) - v(i)) < eps)
      case _ => false
    }
  }
  */



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