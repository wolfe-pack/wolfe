package ml.wolfe.fg20

/**
 * @author riedel
 */
object PotentialDSL extends App {

  trait PotentialPlus[-P1<:Potential, -P2 <: Potential, +R <: Potential] {
    def plus(p1:P1,p2:P2):R
  }

  implicit object DiffPlus extends PotentialPlus[Differentiable,Differentiable,Differentiable] {
    def plus(p1: Differentiable, p2: Differentiable): Differentiable = new DifferentiableSum {
      lazy val args = Seq(p1,p2)
    }
  }
//  implicit object ArgmaxPlus extends PotentialPlus[SupportsArgmax,SupportsArgmax,SupportsArgmax] {
//    def plus(p1: SupportsArgmax, p2: SupportsArgmax) = new Sum[SupportsArgmax] with SupportsArgmax {
//      lazy val args = Seq(p1,p2)
//      def argmaxer(): Argmaxer = ???
//    }
//  }

  implicit object ArgmaxDiffPlus extends PotentialPlus[SupportsArgmax with Differentiable,SupportsArgmax with Differentiable,SupportsArgmax] {
    def plus(p1: SupportsArgmax with Differentiable, p2: SupportsArgmax with Differentiable) = new Sum[SupportsArgmax] with SupportsArgmax {
      lazy val args = Seq(p1,p2)
      def argmaxer(): Argmaxer = ???
    }
  }

  implicit class RichPotential[P <: Potential](val pot:P) {
//    def +(that:P):Potential = new Sum[P] {
//      val args = Seq(pot,that)
//    }
  }

  implicit class RichDifferentiable[P<:Differentiable](val pot:P) {
    def *(scaleValue:Double) = new ScaledDifferentable[P] {
      def scale: Double = scaleValue
      def self = pot
    }

    def observe(state:State) = new DifferentiableWithObservation {
      def self = pot
      lazy val observation = pot.createPartialSetting(state)
    }

    def +(that:Differentiable): Differentiable = new DifferentiableSum {
      val args = Seq(pot,that)
    }

  }

  implicit class RichDifferentiableSupportsArgmax[P <: Differentiable with SupportsArgmax](val pot:P) {
    def *(scaleValue:Double) = new ScaledDifferentable[P] with ScaledSupportsArgmax[P] {
      def scale: Double = scaleValue
      def self = pot
    }
    def +(that:Differentiable with SupportsArgmax):Differentiable with SupportsArgmax = ???
  }

  class ExamplePot1 extends Differentiable {
    override def gradientCalculator(): GradientCalculator = ???
    override def scorer(): Scorer = ???
    override def contVars: Array[ContVar] = ???
    override def discVars: Array[DiscVar[Any]] = ???
    override def vectVars: Array[VectVar] = ???
  }

  class ExamplePot2 extends ExamplePot1 with SupportsArgmax{
    override def argmaxer(): Argmaxer = ???
  }


  val pot1 = new ExamplePot1
  val scaled1 = pot1 * 2.0

  val pot2 = new ExamplePot2

  val scaled2 = pot2 * 2.0

  scaled2.gradientCalculator
  scaled2.argmaxer()

  val add = pot1 + pot2
//  val add2 = pot2 + pot1
//  val add3 = pot2 + pot2
  //val add4 = pot2 + TablePotential(Array.empty, _ => 0.0)







}
