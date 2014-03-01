package ml.wolfe

import scala.language.implicitConversions
import scala.util.Random
import ml.wolfe.Wolfe.Stats.OneHot
import cc.factorie.WeightsSet
import cc.factorie.optimize.{Trainer, OnlineTrainer}

/**
 * @author Sebastian Riedel
 */
object Wolfe extends SampleSpaceDefs with StatsDefs with VectorDefs {

  //core operators

  @Operator.Sum
  def sum[T, N](data: Iterable[T])(predicate: T => Boolean)(obj: T => N)(implicit num: Numeric[N]): N = {
    data.filter(predicate).map(obj).sum(num)
  }

  @Operator.Argmax
  def argmax[T, N](data: Iterable[T])(predicate: T => Boolean)(obj: T => N)(implicit ord: Ordering[N]): T = {
    data.filter(predicate).maxBy(obj)(ord)
  }

  //derived operators

  def argmin[T, N](data: Iterable[T])(predicate: T => Boolean)(obj: T => N)(implicit ord: Ordering[N]): T = {
    argmax(data)(predicate)(obj)(ord.reverse)
  }

  def max[T, N](data: Iterable[T])(predicate: T => Boolean)(obj: T => N)(implicit ord: Ordering[N]): N = {
    obj(argmax(data)(predicate)(obj)(ord))
  }

  def logZ[T](data: Iterable[T])(predicate: T => Boolean)(obj: T => Double): Double = {
    math.log(sum(data)(predicate)(t => math.exp(obj(t))))
  }

  //sufficient statistics

  @deprecated("Use the new operators", "now")
  def sumOld[T, S](elems: Iterable[T])(f: T => S)(implicit num: Numeric[S]) = elems.map(f).sum(num)

  @deprecated("Use the new operators", "now")
  def maxOld[T](elems: Iterable[T])(f: T => Double) = elems.map(f).max

  @deprecated("Use the new operators", "now")
  def argmaxOld[T](dom: Iterable[T])(obj: T => Double): T = {
    dom.maxBy(obj)
  }

  implicit val random = new Random(0)

  def sample[T](dom: Iterable[T])(obj: T => Double)(implicit r: Random = random): T = {
    import cc.factorie._
    dom.view.sampleExpProportionally(obj)(r)
  }

  @Operator.Sample
  def samples[T](num: Int)(dom: Iterable[T])(obj: T => Double)(implicit r: Random = random): Seq[T] = (0 until num).map(x => sample(dom)(obj)(r))

  @deprecated("Use the new operators", "now")
  def argminOld[T](dom: Iterable[T])(obj: T => Double): T = {
    dom.minBy(obj)
  }

  /**
   * Body will get replaced by Frankenwolfe with its LaTeX representation
   */
  @Output.LaTeX
  def toLaTeX(body: () => Unit) = """\LaTeX"""


  @Domain.Simplex
  def simplex[T](domain: Iterable[T], range: Iterable[Double] = doubles) =
    for (p <- maps(domain, range); if sumOld(domain.toSeq) {p(_)} == 1.0 && domain.forall(p(_) >= 0.0)) yield p

  def wsum[T](dom: Iterable[T], stats: T => Vector)(obj: T => Double) = sumOld(dom.toSeq) {x => stats(x) * obj(x)}(VectorNumeric)
  def expect[T](dom: Iterable[T], stats: T => Vector)(obj: T => Double) = wsum(dom, stats)(t => math.exp(obj(t) - logZOld(dom)(obj)))

  @deprecated("Use the new operators", "now")
  def logZOld[T](dom: Iterable[T])(model: T => Double) = math.log(dom.view.map(x => math.exp(model(x))).sum)

  @Operator.Forall
  def forall[T](dom: Iterable[T])(pred: T => Boolean) = dom.forall(pred)

  def allOld[T] = new All[T]


  case class RichCurried[A1, A2, B](f: A1 => A2 => B) {
    def apply(pair: (A2, A1)) = f(pair._2)(pair._1)
  }

  implicit def toRichCurried[A1, A2, B](f: A1 => A2 => B) = new RichCurried(f)

  implicit class BarBuilder[T](t: T) {
    def |[A](that: A) = t -> that
  }


  implicit class RichBoolean(b: Boolean) {
    def -->(that: Boolean) = !b || that
    def <->(that: Boolean) = b == that
  }

  def I(b: Boolean) = if (b) 1.0 else 0.0

  implicit class RichPredicate[T](pred: Map[T, Boolean]) {
    def only(trueAtoms: T*) = {
      val set = trueAtoms.toSet
      pred forall {
        case (a, t) => set(a) == t
      }
    }
  }


  @Objective.LinearModel
  def linearModel[T](featureGenerator: T => List[Vector]) = {
    (example: T, weights: Vector) => weights dot featureGenerator(example).sum
  }

  import scala.annotation._


  object Operator {

    class Argmax extends StaticAnnotation

    class Argmin extends StaticAnnotation

    class Sum extends StaticAnnotation

    class Max extends StaticAnnotation

    class Forall extends StaticAnnotation

    class Sample extends StaticAnnotation


  }

  object Objective {

    trait InferenceSetting

    trait GradientBasedOptimizerSetting

    trait Differentiator

    case class Adagrad(rate: Double) extends GradientBasedOptimizerSetting
    case class Learner(learner: WeightsSet => Trainer) extends GradientBasedOptimizerSetting

    case object Perceptron extends GradientBasedOptimizerSetting


    case class MaxProduct(iterations: Int) extends InferenceSetting

    case object SymPy extends Differentiator

    case object Wolferine extends Differentiator

    class LogLikelihood extends StaticAnnotation

    class JointLoglikelihood extends StaticAnnotation

    class Differentiable(setting: GradientBasedOptimizerSetting = Adagrad(1.0), iterations: Int = 5) extends StaticAnnotation

    class LinearModel(setting: InferenceSetting = MaxProduct(1)) extends StaticAnnotation

    class Categorical extends StaticAnnotation

    class GLM extends StaticAnnotation

    class LogZ extends StaticAnnotation

  }

  object Domain {

    class PMF extends StaticAnnotation

    class Maps extends StaticAnnotation

    class Seqs extends StaticAnnotation

    class Simplex extends StaticAnnotation

    class Marginals extends StaticAnnotation

  }

  object Stats {

    class Categorial extends StaticAnnotation

    class OneHot extends StaticAnnotation

  }

  object Output {

    class LaTeX extends StaticAnnotation

    class SymPy extends StaticAnnotation

  }

}

trait StatsDefs {
  @OneHot
  def oneHot(key: Any, value: Double = 1.0): Wolfe.Vector = Map(key -> value)

}

trait VectorDefs {

  type Vector = Map[Any, Double]


  object Vector {
    def apply(elems: (Any, Double)*) = Map(elems: _*)
  }


  def ft(key: Any, value: Double = 1.0): Vector = Map(key -> value)
  def ft(key: Any, value: Boolean): Vector = ft(key, if (value) 1.0 else 0.0)
  def feat(key: Any*) = Map(key.toSeq.asInstanceOf[Any] -> 1.0)

  val VectorZero = Map.empty[Any, Double]

  implicit object VectorNumeric extends Numeric[Vector] {
    def plus(x: Wolfe.Vector, y: Wolfe.Vector) = {
      val keys = x.keySet ++ y.keySet
      val result = keys map (k => k -> (x.getOrElse(k, 0.0) + y.getOrElse(k, 0.0)))
      result.toMap
    }
    def minus(x: Wolfe.Vector, y: Wolfe.Vector) = ???
    def times(x: Wolfe.Vector, y: Wolfe.Vector) = ???
    def negate(x: Wolfe.Vector) = ???
    def fromInt(x: Int) = ???
    def toInt(x: Wolfe.Vector) = ???
    def toLong(x: Wolfe.Vector) = ???
    def toFloat(x: Wolfe.Vector) = ???
    def toDouble(x: Wolfe.Vector) = ???
    def compare(x: Wolfe.Vector, y: Wolfe.Vector) = ???
    def dot(x: Vector, y: Vector) = {
      x.keys.view.map(k => x(k) * y.getOrElse(k, 0.0)).sum
    }
    override def zero = Map.empty
    def norm(x: Vector) = {
      val sum = x.values.sum
      x mapValues (_ / sum)
    }
  }

  implicit class RichVector(vector: Vector) {

    import Wolfe.{VectorNumeric => num}

    def +(that: Vector) = num.plus(vector, that)
    def dot(that: Vector) = num.dot(vector, that)
    def norm = num.norm(vector)
    def *(scale: Double) = vector.mapValues(_ * scale)
    def *(vector: Vector) = vector.map({case (k, v) => k -> v * vector.getOrElse(k, 0.0)})
  }

}

trait SampleSpaceDefs {

  import Wolfe._

  def all2[A, B](mapper: A => B)(implicit dom: Iterable[A]): Iterable[B] = dom map mapper

  def c[A, B](set1: Iterable[A], set2: Iterable[B]) = for (i <- set1; j <- set2) yield (i, j)
  def c[A, B, C](set1: Iterable[A], set2: Iterable[B], set3: Iterable[C]) = for (i <- set1; j <- set2; k <- set3) yield (i, j, k)
  def c[A1, A2, A3, A4](set1: Iterable[A1], set2: Iterable[A2], set3: Iterable[A3], set4: Iterable[A4]) =
    for (a1 <- set1; a2 <- set2; a3 <- set3; a4 <- set4) yield (a1, a2, a3, a4)
  def c[A1, A2, A3, A4, A5](set1: Iterable[A1], set2: Iterable[A2], set3: Iterable[A3], set4: Iterable[A4], set5: Iterable[A5]) =
    for (a1 <- set1; a2 <- set2; a3 <- set3; a4 <- set4; a5 <- set5) yield (a1, a2, a3, a4, a5)


  type Pred[A] = Map[A, Boolean]

  implicit def Pred[A](implicit dom: Iterable[A]): Iterable[Pred[A]] = preds(dom)
  implicit def Cross2[A1, A2](implicit dom1: Iterable[A1], dom2: Iterable[A2]): Iterable[(A1, A2)] = c(dom1, dom2)
  implicit def Cross3[A1, A2, A3](implicit dom1: Iterable[A1], dom2: Iterable[A2], dom3: Iterable[A3]): Iterable[(A1, A2, A3)] = c(dom1, dom2, dom3)

  @Domain.Maps
  def maps[A, B](dom: Iterable[A], range: Iterable[B]): Iterable[Map[A, B]] = {
    def recurse(d: List[A], r: List[B], funs: List[Map[A, B]] = List(Map.empty)): List[Map[A, B]] = d match {
      case Nil => funs
      case head :: tail =>
        val newFunctions = for (value <- r; f <- funs.view) yield f + (head -> value)
        recurse(tail, r, newFunctions)
    }
    recurse(dom.toList, range.toList).toIterable
  }

  def preds[A](dom: Iterable[A]) = maps(dom, bools)

  def map[A, B](default: B, vals: (A, B)*): Map[A, B] = Map(vals: _*).withDefaultValue(default)
  def map[A, B](keys: Set[A], default: B, vals: (A, B)*): Map[A, B] = (keys -- vals.map(_._1)).map(_ -> default).toMap ++ Map(vals: _*)

  def vectors[A, B](dom: Iterable[A], range: Iterable[B]): Iterable[Map[Any, B]] = {
    def recurse(d: List[A], r: List[B], funs: List[Map[Any, B]] = List(Map.empty)): List[Map[Any, B]] = d match {
      case Nil => funs
      case head :: tail =>
        val newFunctions = for (value <- r; f <- funs.view) yield f + (head -> value)
        recurse(tail, r, newFunctions)
    }
    recurse(dom.toList, range.toList).toIterable
  }


  def seqs[A](dom: Iterable[A], length: Int): Iterable[Seq[A]] = ???

  def seqs[A](doms: Seq[Iterable[A]]): Iterable[Seq[A]] = {
    def recurse(list: List[Iterable[A]], result: Iterable[List[A]] = Iterable(Nil)): Iterable[List[A]] = list match {
      case Nil => result
      case head :: tail =>
        val current = for (r <- result; h <- head) yield h :: r
        recurse(tail, current)
    }
    recurse(doms.toList).map(_.toSeq)
  }

  implicit val bools = Iterable(false, true)

  implicit val doubles: Iterable[Double] = new All[Double]

  implicit val ints: Iterable[Int] = new All[Int]

  val strings: Iterable[String] = new All[String]

  implicit val vectors = new All[Wolfe.Vector]


  case class RichIterable[T](set: Iterable[T]) {
    def ->[B](that: Iterable[B]) = maps(set, that)
  }

  implicit def toRichIterable[T](set: Iterable[T]) = RichIterable(set)

  implicit def toSeq[T](seq: Iterable[T]) = seq.toSeq

  class All[T] extends Iterable[T] {
    def iterator = sys.error("Can't iterate over all objects")
  }

  implicit def unwrap2[A1, A2, B](f: (A1, A2) => B): ((A1, A2)) => B =
    p => f(p._1, p._2)
  implicit def unwrap3[A1, A2, A3, B](f: (A1, A2, A3) => B): ((A1, A2, A3)) => B =
    p => f(p._1, p._2, p._3)
  implicit def unwrap4[A1, A2, A3, A4, B](f: (A1, A2, A3, A4) => B): ((A1, A2, A3, A4)) => B =
    p => f(p._1, p._2, p._3, p._4)
  implicit def unwrap5[A1, A2, A3, A4, A5, B](f: (A1, A2, A3, A4, A5) => B): ((A1, A2, A3, A4, A5)) => B =
    p => f(p._1, p._2, p._3, p._4, p._5)


}


object WolfeEnv extends SampleSpaceDefs with StatsDefs with VectorDefs
