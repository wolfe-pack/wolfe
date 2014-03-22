package ml.wolfe.macros

import ml.wolfe.Wolfe
import Wolfe._

/**
 * @author Sebastian Riedel
 */
class MetaStructuredFactorSpecs extends StructureIsomorphisms {

  "A MetaStructuredFactor" should {
    "provide an atomic table factor" in {
      val space = Seq(false, true)
      val factor = MetaStructuredFactor.structuredFactor[Boolean](space, x => Wolfe.I(x))
      factor mustBeIsomorphicTo (Wolfe.I(_))
      factor.factors.size should be(1)
      factor.arguments.size should be(1)
    }

    "factorize a sum" in {
      case class Data(x: Boolean, y: Boolean, z: Boolean)
      val space = Wolfe.all(Data)
      def potential(d: Data) = Wolfe.I(d.x) + Wolfe.I(d.y) + Wolfe.I(d.z)
      val factor = MetaStructuredFactor.structuredFactor[Data](space, potential)
      factor mustBeIsomorphicTo potential
      factor.factors.size should be(3)
    }

    "generate only one factor for atomic sub-objectives" in {
      case class Data(x: Boolean, y: Boolean, z: Boolean)
      val space = Wolfe.all(Data)
      @OptimizedWolfe.Atomic
      def sub(d: Data) = Wolfe.I(d.x) + Wolfe.I(d.y)
      def potential(d: Data) = sub(d) + Wolfe.I(d.z)
      val factor = MetaStructuredFactor.structuredFactor[Data](space, potential)
      factor mustBeIsomorphicTo potential
      factor.factors.size should be(2)
    }
    "generate a first order sum factor" in {
      implicit val ints = Range(0, 5)
      val space = Wolfe.Pred[Int]
      def potential(pred: Pred[Int]) = ints.map(i => I(pred(i))).sum
      val factor = MetaStructuredFactor.structuredFactor[Pred[Int]](space, potential)
      factor mustBeIsomorphicTo potential
      factor.factors.size should be(5)
    }

    "generate a linear factor" in {
      val space = Range(0,5)
//      val weights = oneHot(1,2.0)
//      def potential(y:Int) = oneHot(y) dot weights
//      val factor = MetaStructuredFactor.structuredFactor[Int](space, potential)
    }


  }

}
