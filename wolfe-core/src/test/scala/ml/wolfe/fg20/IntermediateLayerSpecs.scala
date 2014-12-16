package ml.wolfe.fg20

import cc.factorie.Factorie.DenseTensor1
import ml.wolfe.{FactorieVector, Wolfe, WolfeSpec}
import org.scalatest.FlatSpec

/**
 * @author Sebastian Riedel
 */
class IntermediateLayerSpecs extends WolfeSpec {

  import ml.wolfe.Wolfe._

  def doublePairSpace = ProductSearchSpace2[Double, Double, AtomicSearchSpace.Cont, AtomicSearchSpace.Cont](
    AtomicSearchSpace.cont("x"), AtomicSearchSpace.cont("y"))

  //f(x) = -x^2 + 4x
  trait SimpleSum extends Sum[Differentiable] {
    def space: ProductSearchSpace2[Double, Double, AtomicSearchSpace.Cont, AtomicSearchSpace.Cont, (Double, Double)]
    val args = Seq(
      new QuadraticTerm(space.space1.variable, -1.0),
      new LinearTerm(space.space1.variable, 4.0),
      new QuadraticTerm(space.space2.variable, -1.0),
      new LinearTerm(space.space2.variable, 4.0))
  }


  "A Sum" should {
    "calculate its gradient for continuous variables" in {
      val pairSpace = doublePairSpace
      val sum = new SimpleSum with DifferentiableSum[Differentiable] {
        def space = pairSpace
      }
      val current = State(Map(pairSpace.space1.variable -> 1.0, pairSpace.space2.variable -> 3.0)) //-2*x + 4
      val gradient = Gradient(pairSpace, current)(sum)
      gradient should be((2.0, -2.0))
    }
    "calculate its argmax for differentiable arguments using a gradient based methos" in {
      val pairSpace = doublePairSpace
      val sum = new SimpleSum with SupportsArgmax {
        def space = pairSpace
        def argmaxer(): Argmaxer = new GradientBasedArgmaxer(this)
      }
      val condition = State(Map(pairSpace.space2.variable -> 1.0))
      val result = Argmax(pairSpace, condition)(sum)
      result._1 should be(2.0 +- 0.01)
      result._2 should be(1.0)
    }
  }

  "A potential with observation" should {
    "calculate its gradient" in {
      //todo: tangle variables for more coverage
      val ySpace = AtomicSearchSpace.cont("y")
      val observed = new DifferentiableWithObservation {
        lazy val observation = pairSpace.toPartialSetting(State.single(xSpace.variable, 2.0))
        lazy val xSpace      = AtomicSearchSpace.cont("x")
        lazy val pairSpace   = ProductSearchSpace2[Double, Double, AtomicSearchSpace.Cont, AtomicSearchSpace.Cont](xSpace, ySpace)
        lazy val self        = new SimpleSum with DifferentiableSum[Differentiable] {
          def space = pairSpace
        }
      }
      val at = State(Map(ySpace.variable -> 1.0))
      val result = Gradient(ySpace, at)(observed)
      result should be(2.0)
    }
  }

  "A MaxPotential" should {
    "calculate its gradient" in {
      val labels = AtomicSearchSpace.disc("labels", Seq(false, true))
      val weights = AtomicSearchSpace.vect("weight", 1)
      def feat(value: Boolean) = new DenseTensor1(Array(if (value) 1.0 else -1.0))

      def classifier[T](label: AtomicSearchSpace.Disc[T], weights: AtomicSearchSpace.Vect, feat: T => FactorieVector) =
        new LinearClassiferPotential(label, weights, feat)

      def maxPot(weights: AtomicSearchSpace.Vect) =
        new DifferentiableMaxPotential[LinearClassiferPotential[Boolean]] {
          lazy val objective = classifier(labels, weights, feat)
          def notOptimized = weights
        }

      val result1 = Gradient(weights, State.single(weights.variable, new DenseTensor1(Array(1.0))))(maxPot(weights))
      val result2 = Gradient(weights, State.single(weights.variable, new DenseTensor1(Array(-1.0))))(maxPot(weights))

      result1(0) should be (1.0)
      result2(0) should be (-1.0)

    }
  }


  "The argmax operator" should {
    "support a table potential and a case class search space" in {
      case class XY(x: String, y: String)
      val space = new ProductSearchSpace2(
        new AtomicSearchSpace[String, DiscVar[String]](new DiscVar(Seq("painter"))),
        new AtomicSearchSpace[String, DiscVar[String]](new DiscVar(Seq("noun", "verb"))),
        (x: String, y: String) => XY(x, y), (xy: XY) => xy.x, (xy: XY) => xy.y)

      //def model(xy:XY) = I(xy.x == "painter" && xy.y == "noun")

      val model = TablePotential(
        Array(space.space1.variable, space.space2.variable),
        s => I(s.disc(0) == 0 && s.disc(1) == 0))

      val result = Argmax(space)(model)
      result should be(XY("painter", "noun"))
    }
    "support a flat sum" ignore {
      def bool(name: String) = new AtomicSearchSpace.Disc[Boolean](new DiscVar(Seq(false, true), name))
      val space = new IndexedSeqSearchSpace[Boolean, AtomicSearchSpace.Disc[Boolean]](3, bool)
      val arg1 = TablePotential(Array(space.seq(0).variable, space.seq(1).variable), s => I(s.disc(0) == s.disc(1)))
      val arg2 = TablePotential(Array(space.seq(1).variable, space.seq(2).variable), s => I(s.disc(0) == s.disc(1)))
      val bias = TablePotential(Array(space.seq(0).variable), s => I(s.disc(0) == 1))
      val model = new FlatSum(Seq(arg1, arg2, bias)) {
        def argmaxer() = new MaxProduct(???)
      }
    }

    //    "support a perceptron loss" ignore {
    //
    //      val labels = new AtomicSearchSpace.Disc[Boolean](new DiscVar(Seq(false, true)))
    //      val weights = new AtomicSearchSpace.Vect(new VectVar(1, "weights"))
    //
    //      def feat(value: Boolean) = new DenseTensor1(Array(if (value) 1.0 else 0.0))
    //
    //      def classifier[T](label: AtomicSearchSpace.Disc[T], weights: AtomicSearchSpace.Vect, feat: T => FactorieVector) =
    //        new LinearClassiferPotential(label, weights, feat)
    //
    //      def maxPot(weights: AtomicSearchSpace.Vect) =
    //        new DifferentiableMaxPotential[LinearClassiferPotential[Boolean]] {
    //          val objective = classifier(labels, weights, feat)
    //          def notOptimized = weights
    //        }
    //
    //      def observed(weights: AtomicSearchSpace.Vect)(label: Boolean) =
    //        new DifferentiableWithObservation() {
    //          val constLabel  = AtomicSearchSpace.constDisc(label)
    //          val observation = labels.toPartialSetting(State.single(constLabel.variable, label))
    //          val self        = classifier[Boolean](constLabel, weights, feat)
    //        }
    //
    //      def negLoss(weights: AtomicSearchSpace.Vect) =
    //        new Sum[Differentiable] with SupportsArgmax {
    //          val args = Seq(
    //            maxPot(weights),
    //            ScaledPotential.scaleDifferentiable(observed(weights)(true), -1.0))
    //          def argmaxer() = new GradientBasedArgmaxer(this)
    //        }
    //
    //      val toMaximize = negLoss(weights)
    //
    //      val result = Argmax(weights)(toMaximize)
    //
    //
    //
    //
    //
    //
    //
    //      //given an instance model, calculate its argmax and return statistics of winning settings for exp fam potentials
    //      //inner potential needs a stats(incoming,result) method
    //      //this is used to calculate gradients
    //      //then gradient-based optimizer is
    //      //def classifier(weights:AtomicSearchSpace.Vect, feats:AtomicSearchSpace.Vect, label:AtomicSearchSpace.Disc[T]) = ...
    //      //val inner = new LinearPotential(labelSpace,weightSpace) ...
    //      //val max = new MaxPotential(weightSpace,classifier) //needs SupportsArgmax inner potential.
    //      // max potentials maxes all variables of the inner potentials different to the variables of the max potential.
    //      //val loss = new Minus(max,inner.condition(label = gold))
    //      //linearPotential.gradientCalculator().gradient(observeHiddenVars) should be doable.
    //      //or val loss = new Minus(max, new LinearPotential(restrictedSearchSpace,weightSpace)?
    //      //val result = Argmax(weightsSpace)(loss)
    //      //trait Potential { def observed:PartialSetting = UnobservedSetting(...) }
    //    }



    "support a projective tree potential" ignore {
      def bool(name: String) = new AtomicSearchSpace[Boolean, DiscVar[Boolean]](new DiscVar(Seq(false, true), name))
      def double(name: String) = new AtomicSearchSpace[Double, ContVar](new ContVar(name))
      val slen = 5
      val edges = for (h <- 0 until slen; m <- 1 until slen; if h != m) yield (h, m)
      val treeSpace = new GraphSearchSpace.Disc[Boolean](0 until slen, 1 until slen, bool)
      val scoreSpace = new GraphSearchSpace.Cont(0 until slen, 1 until slen, double)
      val model = new ProjectiveTreePotential(slen, treeSpace, scoreSpace)

      //def model(xy:XY) = treePotential(slen,graph,scores)
      Argmax(treeSpace, scoreSpace.observation(Map((0, 0) -> 2.3)))(model)


    }

  }

}


class Test extends FlatSpec {


}
