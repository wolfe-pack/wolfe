package ml.wolfe

import ml.wolfe.FactorGraph.Node
import org.sameersingh.htmlgen.HTML

import scala.language.implicitConversions
import scala.util.Random
import cc.factorie.model.WeightsSet
import cc.factorie.optimize.Trainer
import scala.annotation.StaticAnnotation
import scala.collection.mutable.{ArrayBuffer, Buffer}
import scala.collection.MapProxy
import math._

/**
 * @author Sebastian Riedel
 */
object Wolfe extends SampleSpaceDefs
                     with StatsDefs
                     with VectorDefs
                     with DefaultValues
                     with ProblemBuilder
                     with Annotations {

  //core operators
  def notSupported = sys.error("This function is not supported")


  //sufficient statistics

  implicit val random = new Random(0)

  def sample[T](dom: Iterable[T])(obj: T => Double)(implicit r: Random = random): T = {
    import cc.factorie._
    dom.view.sampleExpProportionally(obj)(r)
  }

  def samples[T](num: Int)(dom: Iterable[T])(obj: T => Double)(implicit r: Random = random): Seq[T] = (0 until num).map(x => sample(dom)(obj)(r))

  /**
   * Body will get replaced by Frankenwolfe with its LaTeX representation
   */
  def toLaTeX(body: () => Unit) = """\LaTeX"""


  //  @Domain.Simplex
  //  def simplex[T](domain: Iterable[T], range: Iterable[Double] = doubles) =
  //    for (p <- maps(domain, range); if sumOld(domain.toSeq) {p(_)} == 1.0 && domain.forall(p(_) >= 0.0)) yield p
  //
  //  def wsum[T](dom: Iterable[T], stats: T => Vector)(obj: T => Double) = sumOld(dom.toSeq) {x => stats(x) * obj(x)}(VectorNumeric)
  //  def expect[T](dom: Iterable[T], stats: T => Vector)(obj: T => Double) = wsum(dom, stats)(t => math.exp(obj(t) - logZOld(dom)(obj)))

  @deprecated("Use the new operators", "now")
  def logZOld[T](dom: Iterable[T])(model: T => Double) = math.log(dom.view.map(x => math.exp(model(x))).sum)

  case class RichCurried[A1, A2, B](f: A1 => A2 => B) {
    def apply(pair: (A2, A1)) = f(pair._2)(pair._1)
  }

  implicit def toRichCurried[A1, A2, B](f: A1 => A2 => B) = new RichCurried(f)

  def atomic(f: Double) = f
  def atomic(v: Vector) = v


  implicit class BarBuilder[T](t: T) {
    def |[A](that: A) = t -> that
  }


  implicit class RichBoolean(b: Boolean) {
    def -->(that: Boolean) = !b || that
    def <->(that: Boolean) = b == that
  }

  def bernoulli(p: Double = 0.5)(coin: Boolean) = if (coin) log(p) else log1p(-p)

  object logDist {

    def gaussian(mean: Double = 0.0, dev: Double = 1.0)(x: Double) = {
      def sq(x: Double) = x * x
      log(1.0 / (dev * sqrt(2.0 * Pi)) * exp(-sq(x - mean) / (2.0 * sq(dev))))
    }

    def sampleGaussian(mean: Double = 0.0, dev: Double = 1.0) = {
      Random.nextGaussian() * dev + mean
    }

  }


  def I(b: Boolean) = if (b) 1.0 else 0.0
  def logI(b: Boolean) = if (b) 0.0 else Double.NegativeInfinity

  implicit class RichPredicate[T](pred: Map[T, Boolean]) {
    def only(trueAtoms: T*) = {
      val set = trueAtoms.toSet
      pred forall {
        case (a, t) => set(a) == t
      }
    }
  }

  def linearModel[T](featureGenerator: T => List[Vector]) = {
    (example: T, weights: Vector) => weights dot featureGenerator(example).sum
  }

  trait FactorGraphBuffer {
    def set(fg: FactorGraph): Unit
    def get(): FactorGraph
  }
  implicit val FactorGraphBuffer: FactorGraphBuffer = new FactorGraphBuffer {
    var factorGraph: FactorGraph = null
    def set(fg: FactorGraph) = factorGraph = fg
    def get() = factorGraph
  }

}

trait StatsDefs {
  def oneHot(key: Any, value: Double = 1.0): Wolfe.Vector = Wolfe.Vector(key -> value)

}

trait VectorDefs {

  //type Vector = Map[Any, Double]


  class Vector(underlying: Map[Any, Double]) extends scala.collection.immutable.MapProxy[Any, Double] {
    val self = underlying withDefaultValue 0.0

    def +(that: Vector): Vector =
      if (self.size >= that.size) plus(this, that)
      else plus(that, this)

    def +(that: Vector, scale: Double): Vector =
      this + new Vector(that.mapValues(_ * scale))

    def dot(that: Vector) = VectorNumeric.dot(this, that)
    def norm = VectorNumeric.norm(this)
    def *(scale: Double) = new Vector(self.mapValues(_ * scale))
    def *(vector: Vector) = new Vector(vector.self.map({ case (k, v) => k -> v * vector(k) }))
    override def toString() = s"""Vector(${ underlying.map(p => p._1 + " -> " + p._2).mkString(", ") })"""
    override def equals(that: Any) = that match {
      case v: Vector =>
        val keys = v.keySet ++ keySet
        keys.forall(k => this(k) == v(k))
      case _ => false
    }
    def outer(that: Vector) = {
      val map = for ((k1, v1) <- this; (k2, v2) <- that) yield (k1, k2) -> v1 * v2
      new Vector(map.toMap)
    }
    def x(that: Vector) = outer(that)
    override def filter(p: ((Any, Double)) => Boolean): Vector = new Vector(underlying.filter(p))
    override def filterNot(p: ((Any, Double)) => Boolean): Vector = new Vector(underlying.filterNot(p))
    override def groupBy[K](f: ((Any, Double)) => K): Map[K, Vector] = super.groupBy(f).mapValues(m => new Vector(m))
    override def filterKeys(p: (Any) => Boolean): Vector = new Vector(underlying.filterKeys(p))
    def filterKeysWith(p: PartialFunction[Any, Boolean]): Vector =
      new Vector(underlying.filterKeys(p.orElse[Any, Boolean](PartialFunction(x => false))))


  }

  object Vector {
    def apply(elems: (Any, Double)*) = new Vector(Map(elems: _*))
  }

  val VectorZero = new Vector(Map.empty[Any, Double])

  implicit def toVector(map: Map[_, Double]) = new Vector(map.asInstanceOf[Map[Any, Double]])

  implicit object VectorNumeric extends Numeric[Vector] {
    def plus(x: Vector, y: Vector) = x + y
    def minus(x: Vector, y: Vector) = ???
    def times(x: Vector, y: Vector) = ???
    def negate(x: Vector) = ???
    def fromInt(x: Int) = ???
    def toInt(x: Vector) = ???
    def toLong(x: Vector) = ???
    def toFloat(x: Vector) = ???
    def toDouble(x: Vector) = ???
    def compare(x: Vector, y: Vector) = ???
    def dot(x: Vector, y: Vector) = {
      x.self.keys.view.map(k => x(k) * y.self.getOrElse(k, 0.0)).sum
    }
    override val zero = new Vector(Map.empty)
    def norm(x: Vector) = {
      val sum = x.self.values.sum
      x.self.mapValues(_ / sum)
    }
  }

  //  implicit class RichVector(vector: Vector) {
  //
  //    import Wolfe.{VectorNumeric => num}
  //
  //    def +(that: Vector) = num.plus(vector, that)
  //    def dot(that: Vector) = num.dot(vector, that)
  //    def norm = num.norm(vector)
  //    def *(scale: Double) = vector.mapValues(_ * scale)
  //    def *(vector: Vector) = vector.map({ case (k, v) => k -> v * vector.getOrElse(k, 0.0) })
  //  }

  /**
   * Adds two Wolfe vectors by iterating only over the elements of the second vector.
   * @param v1 a large vector.
   * @param v2 a small vector.
   * @return v1+v2.
   */
  private def plus(v1: Vector, v2: Vector): Vector =
    v2.foldLeft(v1)((acc, t) => {
      val (key, value) = t
      new Vector(acc.updated(key, acc.getOrElse(key, 0.0) + value))
    })
}

trait SampleSpaceDefs {
  def all[A, B](mapper: A => B)(implicit dom: Iterable[A]): Iterable[B] = dom map mapper

  def c[A, B](set1: Iterable[A], set2: Iterable[B]) = for (i <- set1; j <- set2) yield (i, j)
  def c[A, B, C](set1: Iterable[A], set2: Iterable[B], set3: Iterable[C]) = for (i <- set1; j <- set2; k <- set3) yield (i, j, k)
  def c[A1, A2, A3, A4](set1: Iterable[A1], set2: Iterable[A2], set3: Iterable[A3], set4: Iterable[A4]) =
    for (a1 <- set1; a2 <- set2; a3 <- set3; a4 <- set4) yield (a1, a2, a3, a4)
  def c[A1, A2, A3, A4, A5](set1: Iterable[A1], set2: Iterable[A2], set3: Iterable[A3], set4: Iterable[A4], set5: Iterable[A5]) =
    for (a1 <- set1; a2 <- set2; a3 <- set3; a4 <- set4; a5 <- set5) yield (a1, a2, a3, a4, a5)

  implicit class CartesianProductBuilder[T1](iter1: Iterable[T1]) {
    def x[T2](iter2: Iterable[T2]) = CartesianProduct2(iter1, iter2)
  }

  case class CartesianProduct2[T1, T2](iter1: Iterable[T1], iter2: Iterable[T2]) extends Iterable[(T1, T2)] {
    def iterator = for (i1 <- iter1.iterator; i2 <- iter2.iterator) yield (i1, i2)
    def x[T3](iter3: Iterable[T3]) = CartesianProduct3(iter1, iter2, iter3)
  }
  case class CartesianProduct3[T1, T2, T3](iter1: Iterable[T1], iter2: Iterable[T2], iter3: Iterable[T3]) extends Iterable[(T1, T2, T3)] {
    def iterator = for (i1 <- iter1.iterator; i2 <- iter2.iterator; i3 <- iter3.iterator) yield (i1, i2, i3)
    def x[T4](iter4: Iterable[T4]) = CartesianProduct4(iter1, iter2, iter3, iter4)
  }
  case class CartesianProduct4[T1, T2, T3, T4](iter1: Iterable[T1],
                                               iter2: Iterable[T2],
                                               iter3: Iterable[T3],
                                               iter4: Iterable[T4]) extends Iterable[(T1, T2, T3, T4)] {
    def iterator = for (i1 <- iter1.iterator;
                        i2 <- iter2.iterator;
                        i3 <- iter3.iterator;
                        i4 <- iter4.iterator) yield (i1, i2, i3, i4)
  }


  type Pred[A] = Map[A, Boolean]

  implicit def Pred[A](implicit dom: Iterable[A]): Iterable[Pred[A]] = preds(dom)
  implicit def Cross2[A1, A2](implicit dom1: Iterable[A1], dom2: Iterable[A2]): Iterable[(A1, A2)] = c(dom1, dom2)
  implicit def Cross3[A1, A2, A3](implicit dom1: Iterable[A1], dom2: Iterable[A2], dom3: Iterable[A3]): Iterable[(A1, A2, A3)] = c(dom1, dom2, dom3)
  implicit def Cross4[A1, A2, A3, A4](implicit dom1: Iterable[A1], dom2: Iterable[A2],
                                      dom3: Iterable[A3], dom4: Iterable[A4]): Iterable[(A1, A2, A3, A4)] = c(dom1, dom2, dom3, dom4)
  implicit def Cross5[A1, A2, A3, A4, A5](implicit dom1: Iterable[A1], dom2: Iterable[A2],
                                          dom3: Iterable[A3], dom4: Iterable[A4],
                                          dom5: Iterable[A5]): Iterable[(A1, A2, A3, A4, A5)] = c(dom1, dom2, dom3, dom4, dom5)

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

  def mapWithDefault[A, B](default: B, vals: (A, B)*): Map[A, B] = Map(vals: _*).withDefaultValue(default)
  def mapOverDomain[A, B](keys: Set[A], default: B, vals: (A, B)*): Map[A, B] = (keys -- vals.map(_._1)).map(_ -> default).toMap ++ Map(vals: _*)

  def buildVectors[A, B](dom: Iterable[A], range: Iterable[B]): Iterable[Map[Any, B]] = {
    def recurse(d: List[A], r: List[B], funs: List[Map[Any, B]] = List(Map.empty)): List[Map[Any, B]] = d match {
      case Nil => funs
      case head :: tail =>
        val newFunctions = for (value <- r; f <- funs.view) yield f + (head -> value)
        recurse(tail, r, newFunctions)
    }
    recurse(dom.toList, range.toList).toIterable
  }

  /** Sequences over ''dom'' of fixed length ''length'' **/
  def seqsOfLength[A](length: Int, dom: Iterable[A]): Iterable[Seq[A]] = {
    def recurse(l: Int, postfix: Iterable[List[A]] = Iterable(Nil)): Iterable[List[A]] =
      l match {
        case 0 => postfix
        case n =>
          val newPrefix = for (head <- dom; tail <- postfix) yield head :: tail
          recurse(n - 1, newPrefix)
      }
    recurse(length).map(_.toIndexedSeq)
  }

  /** Sequences over ''dom'' up to length ''maxLength'' **/
  def seqs[A](dom: Iterable[A], maxLength: Int = 1000): Iterable[Seq[A]] = {
    Range(0, maxLength + 1).view.flatMap(seqsOfLength(_, dom)).toList
  }

  def seqs[A](doms: Seq[Iterable[A]]): Iterable[Seq[A]] = {
    def recurse(list: List[Iterable[A]], result: Iterable[List[A]] = Iterable(Nil)): Iterable[List[A]] = list match {
      case Nil => result
      case head :: tail =>
        val current = for (r <- result; h <- head) yield h :: r
        recurse(tail, current)
    }
    recurse(doms.toList).map(_.toIndexedSeq)
  }

  implicit val bools = Iterable(false, true)

  implicit val doubles: Iterable[Double] = new All[Double]

  implicit val ints: Iterable[Int] = new All[Int]

  implicit val strings: Iterable[String] = new All[String]

  implicit val vectors = new All[Wolfe.Vector]

  def infty[T] = new All[T]

  case class RichIterable[T](set: Iterable[T]) {
    def ->[B](that: Iterable[B]) = maps(set, that)
  }

  implicit def toRichIterable[T](set: Iterable[T]) = RichIterable(set)

  implicit def toSeq[T](seq: Iterable[T]) = seq.toSeq

  sealed class All[T] extends Iterable[T] {
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

  //  class Doubles(maxCount:Double = Double.PositiveInfinity) extends Iterable[Double] {
  //
  //    override def take(n: Int) = new Doubles(n.toDouble)
  //    def iterator = {
  //      var count = 0
  //      Iterator.continually({ count += 1; Random.nextGaussian()}).takeWhile()
  //    }
  //  }

}

trait DefaultValues {
  sealed trait Default
  object default extends Default
  object hidden extends Default
  object unknown extends Default
  implicit def toDefaultValue[T <: AnyRef](default: Default) = null
  implicit def toDefaultInt[Int](default: Default) = -1
  implicit def toDefaultBoolean[Boolean](default: Default) = false
  implicit def toDefaultDouble[Double](default: Default) = 0.0
  def default[T <: AnyRef]: T = null.asInstanceOf[T]

}

trait Annotations {
  class OptimizeByLearning(trainer: WeightsSet => Trainer) extends StaticAnnotation
  class OptimizeByInference(inference: FactorGraph => Unit) extends StaticAnnotation
  class LogZByInference(inference: FactorGraph => Unit) extends StaticAnnotation
  class Atomic extends StaticAnnotation
  class Potential(construct: _ => ml.wolfe.fg.Potential) extends StaticAnnotation
  class OutputFactorGraph(buffer: Wolfe.FactorGraphBuffer = Wolfe.FactorGraphBuffer) extends StaticAnnotation

}

trait ProblemBuilder {

  implicit class RichIterable[T](iterable: Iterable[T]) {
    def where(pred: T => Boolean) = iterable filter pred
    def st(pred: T => Boolean) = iterable filter pred
  }

  case class Builder[T, N](dom: Iterable[T],
                           filter: T => Boolean = (_: T) => true,
                           obj: T => N,
                           mapper: T => T = (t: T) => t) {
    def where(where: T => Boolean) = copy(filter = where)
    def subjectTo(st: T => Boolean) = where(st)
    def st(st: T => Boolean) = where(st)
    def over(over: Iterable[T]) = copy(dom = over)
    def of[NewN](of: T => NewN) = Builder[T, NewN](dom, filter, of)
    def apply(of: T => N) = Builder[T, N](dom, filter, of)
    def using(using: T => T) = copy(mapper = using)
  }


  //  implicit def toOverWhereOf[T, N](obj: T => N) = Builder[T, N](Nil, obj = obj)
  //  implicit def toOverWhereOf[T](dom: Iterable[T]) = Builder[T, Double](dom, obj = (_: T) => 0.0)
  //  implicit def toOverWhereOf[T](obj: T => Double) = OverWhereOf[T,Double](Nil, obj = obj)

  def over[T](implicit over: Iterable[T]) = Builder(over, (_: T) => true, (_: T) => 0.0)
  def where[T: Iterable](where: T => Boolean) = Builder(implicitly[Iterable[T]], where, (_: T) => 0.0)
  def obj[T, N](of: T => N) = Builder[T, N](Nil, (_: T) => true, of)

  //  def of[T: Iterable](of: T => Double) = OverWhereOf(implicitly[Iterable[T]], (_: T) => true, of)

}
