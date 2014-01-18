package scalapplcodefest

import scala.language.implicitConversions
import scala.util.Random
import scalapplcodefest.sbt.Analyze
import scalapplcodefest.Wolfe.Stats.OneHot

/**
 * @author Sebastian Riedel
 */
@Analyze
object Wolfe {

  //core operators

  @Operator.Sum
  def sum2[T, N](data: Iterable[T])(predicate: T => Boolean)(obj: T => N)(implicit num: Numeric[N]): N = {
    data.filter(predicate).map(obj).sum(num)
  }

  @Operator.Argmax
  def argmax2[T, N](data: Iterable[T])(predicate: T => Boolean)(obj: T => N)(implicit ord: Ordering[N]): T = {
    data.filter(predicate).maxBy(obj)(ord)
  }

  //derived operators

  def argmin2[T, N](data: Iterable[T])(predicate: T => Boolean)(obj: T => N)(implicit ord: Ordering[N]): T = {
    argmax2(data)(predicate)(obj)(ord.reverse)
  }

  def max2[T, N](data: Iterable[T])(predicate: T => Boolean)(obj: T => N)(implicit ord: Ordering[N]): N = {
    obj(argmax2(data)(predicate)(obj)(ord))
  }

  def logZ2[T](data: Iterable[T])(predicate: T => Boolean)(obj: T => Double): Double = {
    math.log(sum2(data)(predicate)(t => math.exp(obj(t))))
  }

  //sufficient statistics

  @OneHot
  def oneHot(key: Any, value: Double = 1.0): Vector = Map(key -> value)

  @Domain.Maps
  def maps[A, B](dom: Set[A], range: Set[B]): Set[Map[A, B]] = {
    def recurse(d: List[A], r: List[B], funs: List[Map[A, B]] = List(Map.empty)): List[Map[A, B]] = d match {
      case Nil => funs
      case head :: tail =>
        val newFunctions = for (value <- r; f <- funs.view) yield f + (head -> value)
        recurse(tail, r, newFunctions)
    }
    recurse(dom.toList, range.toList).toSet
  }

  def preds[A](dom: Set[A]) = maps(dom, bools)

  def map[A, B](default: B, vals: (A, B)*): Map[A, B] = Map(vals: _*).withDefaultValue(default)
  def map[A, B](keys: Set[A], default: B, vals: (A, B)*): Map[A, B] = (keys -- vals.map(_._1)).map(_ -> default).toMap ++ Map(vals: _*)

  def vectors[A, B](dom: Set[A], range: Set[B]): Set[Map[Any, B]] = {
    def recurse(d: List[A], r: List[B], funs: List[Map[Any, B]] = List(Map.empty)): List[Map[Any, B]] = d match {
      case Nil => funs
      case head :: tail =>
        val newFunctions = for (value <- r; f <- funs.view) yield f + (head -> value)
        recurse(tail, r, newFunctions)
    }
    recurse(dom.toList, range.toList).toSet
  }


  def seqs[A](dom: Set[A], length: Int): Set[Seq[A]] = ???

  @Operator.Sum
  def sum[T, S](elems: Seq[T])(f: T => S)(implicit num: Numeric[S]) = elems.map(f).sum(num)

  @Operator.Max
  def max[T](elems: Set[T])(f: T => Double) = elems.map(f).max

  @Operator.Argmax
  def argmax[T](dom: Set[T])(obj: T => Double): T = {
    dom.maxBy(obj)
  }

  implicit val random = new Random(0)

  def sample[T](dom: Set[T])(obj: T => Double)(implicit r: Random = random): T = {
    import cc.factorie._
    dom.view.sampleExpProportionally(obj)(r)
  }

  @Operator.Sample
  def samples[T](num: Int)(dom: Set[T])(obj: T => Double)(implicit r: Random = random): Seq[T] = (0 until num).map(x => sample(dom)(obj)(r))

  @Operator.Argmin
  def argmin[T](dom: Set[T])(obj: T => Double): T = {
    dom.minBy(obj)
  }

  /**
   * Body will get replaced by Frankenwolfe with its LaTeX representation
   */
  @Output.LaTeX
  def toLaTeX(body: () => Unit) = """\LaTeX"""


  class All[T] extends Set[T] {
    def +(elem: T) = this
    def -(elem: T) = sys.error("Can't remove element from all objects")
    def contains(elem: T) = true
    def iterator = sys.error("Can't iterate over all objects")
  }

  @Domain.Simplex
  def simplex[T](domain: Set[T], range: Set[Double] = doubles) =
    for (p <- maps(domain, range); if sum(domain.toSeq) {p(_)} == 1.0 && domain.forall(p(_) >= 0.0)) yield p

  def wsum[T](dom: Set[T], stats: T => Vector)(obj: T => Double) = sum(dom.toSeq) {x => stats(x) * obj(x)}(VectorNumeric)
  def expect[T](dom: Set[T], stats: T => Vector)(obj: T => Double) = wsum(dom, stats)(t => math.exp(obj(t) - logZ(dom)(obj)))

  @Objective.LogZ
  def logZ[T](dom: Set[T])(model: T => Double) = math.log(dom.view.map(x => math.exp(model(x))).sum)

  @Operator.Forall
  def forall[T](dom: Set[T])(pred: T => Boolean) = dom.forall(pred)

  def all[T] = new All[T]

  val bools = Set(false, true)

  val doubles: Set[Double] = new All[Double]

  val ints: Set[Int] = new All[Int]

  val strings: Set[String] = new All[String]

  case class RichSet[T](set: Set[T]) {
    def ->[B](that: Set[B]) = maps(set, that)
  }

  implicit def toRichSet[T](set: Set[T]) = RichSet(set)

  implicit def toSeq[T](seq: Set[T]) = seq.toSeq

  case class RichCurried[A1, A2, B](f: A1 => A2 => B) {
    def apply(pair: (A2, A1)) = f(pair._2)(pair._1)
  }

  implicit def toRichCurried[A1, A2, B](f: A1 => A2 => B) = new RichCurried(f)

  implicit class BarBuilder[T](t: T) {
    def |[A](that: A) = t -> that
  }

  type Vector = Map[Any, Double]

  implicit class RichBoolean(b: Boolean) {
    def ->(that: Boolean) = !b || that
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

  type Pred[A] = Map[A, Boolean]

  implicit def unwrap2[A1, A2, B](f: (A1, A2) => B): ((A1, A2)) => B =
    p => f(p._1, p._2)
  implicit def unwrap3[A1, A2, A3, B](f: (A1, A2, A3) => B): ((A1, A2, A3)) => B =
    p => f(p._1, p._2, p._3)
  implicit def unwrap4[A1, A2, A3, A4, B](f: (A1, A2, A3, A4) => B): ((A1, A2, A3, A4)) => B =
    p => f(p._1, p._2, p._3, p._4)
  implicit def unwrap5[A1, A2, A3, A4, A5, B](f: (A1, A2, A3, A4, A5) => B): ((A1, A2, A3, A4, A5)) => B =
    p => f(p._1, p._2, p._3, p._4, p._5)


  object Vector {
    def apply(elems: (Any, Double)*) = Map(elems: _*)
  }

  val vectors = new All[Vector]

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
      x.keys.view.map(k => x(k) * y(k)).sum
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

  def c[A, B](set1: Set[A], set2: Set[B]) = for (i <- set1; j <- set2) yield (i, j)
  def c[A, B, C](set1: Set[A], set2: Set[B], set3: Set[C]) = for (i <- set1; j <- set2; k <- set3) yield (i, j, k)
  def c[A1, A2, A3, A4](set1: Set[A1], set2: Set[A2], set3: Set[A3], set4: Set[A4]) =
    for (a1 <- set1; a2 <- set2; a3 <- set3; a4 <- set4) yield (a1, a2, a3, a4)
  def c[A1, A2, A3, A4, A5](set1: Set[A1], set2: Set[A2], set3: Set[A3], set4: Set[A4], set5: Set[A5]) =
    for (a1 <- set1; a2 <- set2; a3 <- set3; a4 <- set4; a5 <- set5) yield (a1, a2, a3, a4, a5)


  def all[A, B](mapper: A => B)(implicit dom: Set[A]): Set[B] = dom map mapper
  implicit def Pred[A](implicit dom: Set[A]): Set[Pred[A]] = preds(dom)
  implicit def Cross2[A1, A2](implicit dom1: Set[A1], dom2: Set[A2]): Set[(A1, A2)] = c(dom1, dom2)
  implicit def Cross3[A1, A2, A3](implicit dom1: Set[A1], dom2: Set[A2], dom3: Set[A3]): Set[(A1, A2, A3)] = c(dom1, dom2, dom3)


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

    case class MaxProduct(iterations: Int) extends InferenceSetting

    case object SymPy extends Differentiator

    case object Wolferine extends Differentiator

    class LogLikelihood extends StaticAnnotation

    class JointLoglikelihood extends StaticAnnotation

    class Differentiable(setting: GradientBasedOptimizerSetting = Adagrad(1.0),
                         differentiator: Option[Differentiator] = None) extends StaticAnnotation

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



