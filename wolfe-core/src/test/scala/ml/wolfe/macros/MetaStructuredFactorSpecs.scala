package ml.wolfe.macros

import ml.wolfe.Wolfe
import Wolfe._
import OptimizedOperators._


/**
 * @author Sebastian Riedel
 */
class MetaStructuredFactorSpecs extends StructureIsomorphisms {

  "A MetaStructuredFactor" should {
    "provide an atomic table factor" in {
      def space = Seq(false, true)
      val factor = MetaStructuredFactor.structuredFactor[Boolean](space, x => Wolfe.I(x))
      factor mustBeIsomorphicTo (Wolfe.I(_))
      factor.factors.size should be(1)
      factor.arguments.size should be(1)
    }

    "factorize a sum " in {
      case class Data(x: Boolean, y: Boolean, z: Boolean)
      def space = Wolfe.all(Data)
      def potential(d: Data) = Wolfe.I(d.x) + Wolfe.I(d.y) + Wolfe.I(d.z)
      val factor = MetaStructuredFactor.structuredFactor[Data](space, potential)
      factor mustBeIsomorphicTo potential
      factor.factors.size should be(3)
    }

    "generate only one factor for atomic sub-objectives" in {
      case class Data(x: Boolean, y: Boolean, z: Boolean)
      def space = Wolfe.all(Data)
      @Atomic
      def sub(d: Data) = Wolfe.I(d.x) + Wolfe.I(d.y)
      def potential(d: Data) = sub(d) + Wolfe.I(d.z)
      val factor = MetaStructuredFactor.structuredFactor[Data](space, potential)
      factor mustBeIsomorphicTo potential
      factor.factors.size should be(2)
    }
    "generate a first order sum factor" in {
      implicit val ints = Range(0, 5)
      def space = Wolfe.Pred[Int]
      def potential(pred: Pred[Int]) = ints.map(i => I(pred(i))).sum
      val factor = MetaStructuredFactor.structuredFactor[Pred[Int]](space, potential)
      factor mustBeIsomorphicTo potential
      factor.factors.size should be(5)
    }

    "generate a first order sum factor using the sum operator" in {
      implicit val ints = Range(0, 5)
      def space = Wolfe.Pred[Int]
      def potential(pred: Pred[Int]) = sum { over(ints) of (i => I(pred(i)))}
      val factor = MetaStructuredFactor.structuredFactor[Pred[Int]](space, potential)
      factor mustBeIsomorphicTo potential
      factor.factors.size should be(5)
    }

    "generate a linear factor" in {
      def space = Range(0, 5)
      def potential(w: Vector)(y: Int) = oneHot(y) dot w
      val weights = oneHot(3, 2.0)
      val factor = MetaStructuredFactor.structuredLinearFactor[Int](space, potential)
      factor(weights) mustBeIsomorphicTo potential(weights)
    }

    "generate a linear factor from a sum of vectors with a weight parameter" in {
      def space = Range(0, 5)
      def features(y: Int) = oneHot(y) + oneHot(y + 1)
      def potential(w: Vector)(y: Int) = features(y) dot w
      val weights = oneHot(3, 2.0)
      val factor = MetaStructuredFactor.structuredLinearFactor[Int](space, potential)
      factor(weights) mustBeIsomorphicTo potential(weights)
      factor(weights).factors.size should be(2)
    }

    "generate a linear factor from a sum of vectors without a weight parameter " in {
      def space = Range(0, 5)
      def features(y: Int) = oneHot(y) + oneHot(y + 1)
      val weights = oneHot(3, 2.0)
      def potential(y: Int) = features(y) dot weights
      val factor = MetaStructuredFactor.structuredFactor[Int](space, potential)
      factor mustBeIsomorphicTo potential
      factor.factors.size should be (2)
    }

    "generate a linear chain" in {
      def space = seqs(5,Range(0,3))
      def potential(seq:Seq[Int]) = sum { over(0 until seq.size) of (seq(_).toDouble)}
      val factor = MetaStructuredFactor.structuredFactor(space,potential)
      factor mustBeIsomorphicTo potential
      factor.factors.size should be (5)
    }

    "generate an atomic factor for first order sum feature because the argument appears in an atomic position" in {
      case class Data(label:Boolean, elements:Seq[Int])
      def space = Wolfe.all(Data)(bools x Seq(Seq(0,5)))
      def potential(data:Data) = sum { over(0 until data.elements.size) of (i => I(data.label) * data.elements(i).toDouble)}
      val factor = MetaStructuredFactor.structuredFactor(space,potential)
      factor mustBeIsomorphicTo potential
      factor.factors.size should be (1)
    }

    "generate a linear chain with local and pairwise factors" in {
      def space = seqs(5, Range(0, 3))
      def potential(seq: Seq[Int]) = {
        val local = sum { over(0 until seq.size) of (seq(_).toDouble) }
        val pairs = sum { over(0 until seq.size - 1) of (i => I(seq(i) == seq(i + 1))) }
        local + pairs
      }
      val factor = MetaStructuredFactor.structuredFactor(space, potential)
      factor mustBeIsomorphicTo potential
      factor.factors.size should be(9)
    }

    "generate a linear chain with local and pairwise factors where defined vals are used within a val defintion" in {
      def space = seqs(5, Range(0, 3))
      def potential(seq: Seq[Int]) = {
        val n = seq.size
        val local = sum { over(0 until n) of (seq(_).toDouble) }
        val pairs = sum { over(0 until n - 1) of (i => I(seq(i) == seq(i + 1))) }
        local + pairs
      }
      val factor = MetaStructuredFactor.structuredFactor(space, potential)
      factor mustBeIsomorphicTo potential
      factor.factors.size should be(9)
    }


    "generate a linear chain with local and pairwise dot-product factors" in {
      def space = seqs(5, Range(0, 3))
      def features(seq: Seq[Int]) = {
        val local = sum { over(0 until seq.size) of (i => oneHot(seq(i))) }
        val pairs = sum { over(0 until seq.size - 1) of (i => oneHot(seq(i) -> seq(i + 1))) }
        local + pairs
      }
      def potential(w: Vector)(s: Seq[Int]) = w dot features(s)
      val w = oneHot(1)
      val factor = MetaStructuredFactor.structuredLinearFactor(space, potential)
      factor(w) mustBeIsomorphicTo potential(w)
      factor(w).factors.size should be(9)
    }


  }

}

