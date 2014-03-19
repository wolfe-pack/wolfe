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
    "factorize a sum " in {
      case class Data(x: Boolean, y: Boolean, z: Boolean)
      val space = Wolfe.all(Data)
      def potential(d: Data) = Wolfe.I(d.x) + Wolfe.I(d.y) + Wolfe.I(d.z)
      val factor = MetaStructuredFactor.structuredFactor[Data](space, potential)
      factor mustBeIsomorphicTo potential
      factor.factors.size should be(3)
    }

  }

}
