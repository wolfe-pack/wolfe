package ml.wolfe.macros

import ml.wolfe.{FactorGraph, Wolfe}
import Wolfe._
import OptimizedOperators._
import ml.wolfe.fg.ExactlyOncePotential


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

    "factorize a sum" in {
      case class Data(x: Boolean, y: Boolean, z: Boolean)
      def space = Wolfe.all(Data)
      def potential(d: Data) = Wolfe.I(d.x) + Wolfe.I(d.y) + Wolfe.I(d.z)
      val factor = MetaStructuredFactor.structuredFactor[Data](space, potential)
      factor mustBeIsomorphicTo potential
      factor.factors.size should be(3)
    }

    "merge factors with the same hidden variables " in {
      case class Data(x: Boolean, y: Boolean, z: Boolean)
      def space = Wolfe.all(Data)
      def potential(d: Data) = Wolfe.I(d.x) + Wolfe.I(d.x) + Wolfe.I(d.y) + Wolfe.I(d.z)
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
      def potential(pred: Pred[Int]) = sum(ints) { i => I(pred(i)) }
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

    "generate a linear factor from a sum of vectors with a weight parameter " in {
      case class Data(x: Int, y: Int)
      def space = Wolfe.all(Data)(Range(0, 5) x Range(0, 5))
      def features(y: Data) = oneHot(y.x) + oneHot(y.y + 1)
      def potential(w: Vector)(y: Data) = features(y) dot w
      val weights = oneHot(3, 2.0)
      val factor = MetaStructuredFactor.structuredLinearFactor[Data](space, potential)
      factor(weights) mustBeIsomorphicTo potential(weights)
      factor(weights).factors.size should be(2)
    }

    "generate a linear factor from a sum of vectors without a weight parameter " in {
      case class Data(x: Int, y: Int)
      def space = Wolfe.all(Data)(Range(0, 5) x Range(0, 5))
      def features(y: Data) = oneHot(y.x) + oneHot(y.y + 1)
      val weights = oneHot(3, 2.0)
      def potential(y: Data) = features(y) dot weights
      val factor = MetaStructuredFactor.structuredFactor[Data](space, potential)
      factor mustBeIsomorphicTo potential
      factor.factors.size should be(2)
    }

    "generate a linear chain" in {
      def space = seqsOfLength(5, Range(0, 3))
      def potential(seq: Seq[Int]) = sum(0 until seq.size) { seq(_).toDouble }
      val factor = MetaStructuredFactor.structuredFactor(space, potential)
      factor mustBeIsomorphicTo potential
      factor.factors.size should be(5)
    }

    "generate an atomic factor for first order sum feature because the argument appears in an atomic position" in {
      case class Data(label: Boolean, elements: Seq[Int])
      def space = Wolfe.all(Data)(bools x Seq(Seq(0, 5)))
      def potential(data: Data) = sum(0 until data.elements.size) { i => I(data.label) * data.elements(i).toDouble }
      val factor = MetaStructuredFactor.structuredFactor(space, potential)
      factor mustBeIsomorphicTo potential
      factor.factors.size should be(1)
    }

    "generate a linear chain with local and pairwise factors " in {
      def space = seqsOfLength(5, Range(0, 3))
      def potential(seq: Seq[Int]) = {
        val local = sum(0 until seq.size) { seq(_).toDouble }
        val pairs = sum(0 until seq.size - 1) { i => I(seq(i) == seq(i + 1)) }
        local + pairs
      }
      val factor = MetaStructuredFactor.structuredFactor(space, potential)
      factor mustBeIsomorphicTo potential
      factor.factors.size should be(9)
    }

    "merge local factors" in {
      def space = seqsOfLength(5, Range(0, 3))
      def potential(seq: Seq[Int]) = {
        val local1 = sum(0 until seq.size) { seq(_).toDouble }
        val local2 = sum(0 until seq.size) { seq(_).toDouble * 2.0 }
        local1 + local2
      }
      val factor = MetaStructuredFactor.structuredFactor(space, potential)
      factor mustBeIsomorphicTo potential
      factor.factors.size should be(5)
    }


    "generate a linear chain with local and pairwise factors where defined vals are used within a val defintion" in {
      def space = seqsOfLength(5, Range(0, 3))
      def potential(seq: Seq[Int]) = {
        val n = seq.size
        val local = sum(0 until n) { seq(_).toDouble }
        val pairs = sum(0 until n - 1) { i => I(seq(i) == seq(i + 1)) }
        local + pairs
      }
      val factor = MetaStructuredFactor.structuredFactor(space, potential)
      factor mustBeIsomorphicTo potential
      factor.factors.size should be(9)
    }


    "generate a linear chain with local and pairwise dot-product factors" in {
      def space = seqsOfLength(5, Range(0, 3))
      def features(seq: Seq[Int]) = {
        val local = sum(0 until seq.size) { i => oneHot(seq(i)) }
        val pairs = sum(0 until seq.size - 1) { i => oneHot(seq(i) -> seq(i + 1)) }
        local + pairs
      }
      def potential(w: Vector)(s: Seq[Int]) = w dot features(s)
      val w = oneHot(1)
      val factor = MetaStructuredFactor.structuredLinearFactor(space, potential)
      factor(w) mustBeIsomorphicTo potential(w)
      factor(w).factors.size should be(9)
    }

    "merge local linear factors" in {
      def space = seqsOfLength(5, Range(0, 3))
      def features(seq: Seq[Int]) = {
        val local1 = sum(0 until seq.size) { i => oneHot('a -> seq(i)) }
        val local2 = sum(0 until seq.size) { i => oneHot('b -> seq(i)) }
        local1 + local2
      }
      def potential(w: Vector)(s: Seq[Int]) = w dot features(s)
      val w = oneHot('a -> 1)
      val factor = MetaStructuredFactor.structuredLinearFactor(space, potential)
      factor(w) mustBeIsomorphicTo potential(w)
      factor(w).factors.size should be(5)
    }

    "create a factor with user-specified potential" in {
      case class Sample(pred: Pred[Int])
      implicit def ints = 0 until 5
      def space = Wolfe.all(Sample)

      @Potential(new ExactlyOncePotential(_: Seq[FactorGraph.Edge]))
      def exactlyOnce(args: Seq[Boolean]) = if (args.count(identity) == 1) 0.0 else Double.NegativeInfinity

      def model(s: Sample) = exactlyOnce(for (i <- 0 until 5) yield s.pred(i))

      val structuredFactor = MetaStructuredFactor.structuredFactor(space, model)
      structuredFactor.factors.size should be(1)
      val factor = structuredFactor.factors.next()
      factor.edges.length should be(5)
      factor.potential shouldBe a[ExactlyOncePotential]

    }

    "create a factors for nested sums " in {
      def ints = 0 until 3
      def space = Wolfe.maps(ints x ints, ints)
      def model(map: Map[(Int, Int), Int]) = sum(ints) { i => sum(ints) { j => I(map(i, j) == 1) } }
      val structuredFactor = MetaStructuredFactor.structuredFactor(space, model)
      structuredFactor mustBeIsomorphicTo model
      structuredFactor.factors.size should be (3 * 3)
    }


  }

}

