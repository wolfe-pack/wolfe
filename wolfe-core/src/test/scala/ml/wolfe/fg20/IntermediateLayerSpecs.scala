package ml.wolfe.fg20

import cc.factorie.Factorie.DenseTensor1
import ml.wolfe.{FactorieConverter, SimpleIndex, FactorieVector, WolfeSpec}
import org.scalatest.FlatSpec

/**
 * @author Sebastian Riedel
 */
class IntermediateLayerSpecs extends WolfeSpec {

  import ml.wolfe.Wolfe._

  def doublePairSpace = ProductDomain2[Double, Double, AtomicDomain.Cont, AtomicDomain.Cont](
    AtomicDomain.cont("x"), AtomicDomain.cont("y"))

  //f(x) = -x^2 + 4x
  trait SimpleSum extends Sum[Differentiable] {
    def space: ProductDomain2[Double, Double, AtomicDomain.Cont, AtomicDomain.Cont, (Double, Double)]
    val args = Seq(
      new QuadraticTerm(space.space1.variable, -1.0),
      new LinearTerm(space.space1.variable, 4.0),
      new QuadraticTerm(space.space2.variable, -1.0),
      new LinearTerm(space.space2.variable, 4.0))
  }

  def feat(value: Boolean) = new DenseTensor1(Array(if (value) 1.0 else -1.0))

  def classifier[T](label: AtomicDomain.Disc[T], weights: AtomicDomain.Vect, feat: T => FactorieVector) =
    new LinearClassiferPotential(label, weights, feat)

  def maxPot(weights: AtomicDomain.Vect, labels: => AtomicDomain.Disc[Boolean]) =
    new DifferentiableMaxPotential[LinearClassiferPotential[Boolean]] {
      lazy val objective = classifier(labels, weights, feat)
      def notOptimized = weights
    }


  "A Sum" should {
    "calculate its gradient for continuous variables" in {
      val pairSpace = doublePairSpace
      val sum = new SimpleSum with DifferentiableSum {
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
      val ySpace = AtomicDomain.cont("y")
      //f(x,y) = x * y + y^2
      val observed = new DifferentiableWithObservation {
        lazy val xSpace      = AtomicDomain.cont("x")
        lazy val self        = new DifferentiableSum {
          lazy val args = Seq(new BilinearTerm(xSpace.variable, ySpace.variable), new QuadraticTerm(ySpace.variable, 1.0))
        }
        lazy val observation = self.createPartialSetting(State.single(xSpace.variable, 2.0))
      }
      val at = State(Map(ySpace.variable -> 1.0))
      val result = Gradient(ySpace, at)(observed)
      result should be(4.0)
    }
  }

  "A perceptron loss" should {
    "calculate its gradient" in {
      def labels = AtomicDomain.disc("labels", IndexedSeq(false, true))
      val weights = AtomicDomain.vect("weights", 1)

      val loss = new DifferentiableSum {
        lazy val arg1 = maxPot(weights, labels)
        lazy val arg2 = new ScaledDifferentable[Differentiable] {
          def scale: Double = -1
          lazy val self = new DifferentiableWithObservation {
            lazy val observedLabel = AtomicDomain.constDisc(true)
            lazy val observation   = self.toPartialSetting(State.single(observedLabel.variable, true))
            lazy val self          = classifier(observedLabel, weights, feat)
          }
        }
        lazy val args = Seq(arg1, arg2)
      }
      val result = Gradient(weights, State.single(weights.variable, new DenseTensor1(Array(-1.0))))(loss)
      result(0) should be(-2.0)
    }
  }

  "A linear chain" should {
    "calculate its argmax" in {
      val index = new SimpleIndex

      def pairwise(weights: AtomicDomain.Vect,
                   labels: IndexedSeqDomain[String, AtomicDomain.Disc[String]],
                   words: IndexedSeq[String]) =
        new DifferentiableSum {
          lazy val args = for (i <- 0 until words.size - 1) yield
            LinearPotential(weights.variable, Array(labels.seq(i).variable, labels.seq(i + 1).variable),
              setting => {
                val label1 = labels.seq(i).variable.dom(setting.disc(0))
                val label2 = labels.seq(i + 1).variable.dom(setting.disc(1))
                FactorieConverter.toFreshFactorieSparseVector(oneHot(label1 -> label2), index)
              })

        }

      def local(weights: AtomicDomain.Vect,
                labels: IndexedSeqDomain[String, AtomicDomain.Disc[String]],
                words: IndexedSeq[String]) =
        new DifferentiableSum {
          lazy val args = for (i <- 0 until words.size) yield
            LinearPotential(weights.variable, Array(labels.seq(i).variable), setting => {
              val label = labels.seq(i).variable.dom(setting.disc(0))
              FactorieConverter.toFreshFactorieSparseVector(oneHot(words(i) -> label), index)
            })
        }

      def crf(weights: AtomicDomain.Vect,
              labels: IndexedSeqDomain[String, AtomicDomain.Disc[String]],
              words: IndexedSeq[String]) = new DifferentiableSum with SupportsArgmax {
        //lazy val args = Seq(local(weights, labels, words), pairwise(weights, labels, words))
        lazy val args = local(weights, labels, words).args ++ pairwise(weights, labels, words).args
        def argmaxer(): Argmaxer = new MaxProduct(Problem(args))
      }

      val weights = AtomicDomain.vect("w")
      val words = IndexedSeq("The", "cat", "sat", "on", "the", "mat")
      val labels = new IndexedSeqDomain[String, AtomicDomain.Disc[String]](
        words.size,
        n => AtomicDomain.disc("label" + n, IndexedSeq("N", "V", "I")))

      val model = crf(weights, labels, words)





    }


  }

  "A MaxPotential" should {
    "calculate its gradient" in {
      def labels = AtomicDomain.disc("labels", IndexedSeq(false, true))
      val weights = AtomicDomain.vect("weight", 1)

      val result1 = Gradient(weights, State.single(weights.variable, new DenseTensor1(Array(1.0))))(maxPot(weights, labels))
      val result2 = Gradient(weights, State.single(weights.variable, new DenseTensor1(Array(-1.0))))(maxPot(weights, labels))

      result1(0) should be(1.0)
      result2(0) should be(-1.0)

    }
  }


  "The argmax operator" should {
    "support a table potential and a case class search space" in {
      case class XY(x: String, y: String)
      val space = new ProductDomain2(
        new AtomicDomain[String, DiscVar[String]](new DiscVar(IndexedSeq("painter"))),
        new AtomicDomain[String, DiscVar[String]](new DiscVar(IndexedSeq("noun", "verb"))),
        (x: String, y: String) => XY(x, y), (xy: XY) => xy.x, (xy: XY) => xy.y)

      //def model(xy:XY) = I(xy.x == "painter" && xy.y == "noun")

      val model = TablePotential(
        Array(space.space1.variable, space.space2.variable),
        s => I(s.disc(0) == 0 && s.disc(1) == 0))

      val result = Argmax(space)(model)
      result should be(XY("painter", "noun"))
    }
    "support a flat sum" ignore {
      def bool(name: String) = new AtomicDomain.Disc[Boolean](new DiscVar(IndexedSeq(false, true), name))
      val space = new IndexedSeqDomain[Boolean, AtomicDomain.Disc[Boolean]](3, bool)
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
      def bool(name: String) = new AtomicDomain[Boolean, DiscVar[Boolean]](new DiscVar(IndexedSeq(false, true), name))
      def double(name: String) = new AtomicDomain[Double, ContVar](new ContVar(name))
      val slen = 5
      val edges = for (h <- 0 until slen; m <- 1 until slen; if h != m) yield (h, m)
      val treeSpace = new GraphDomain.Disc[Boolean](0 until slen, 1 until slen, bool)
      val scoreSpace = new GraphDomain.Cont(0 until slen, 1 until slen, double)
      val model = new ProjectiveTreePotential(slen, treeSpace, scoreSpace)

      //def model(xy:XY) = treePotential(slen,graph,scores)
      Argmax(treeSpace, scoreSpace.toState(Map((0, 0) -> 2.3)))(model)


    }

  }

}


class Test extends FlatSpec {


}
