package scalapplcodefest.example

import scalapplcodefest.compiler.{WolfeTransformer, StringCompiler}
import scala.language.implicitConversions
import scala.tools.nsc.Global

/**
 * @author Sebastian Riedel
 */
object MLECompileExample {

  val source =
    """
      |package scalapplcodefest.example
      |
      |import math.log
      |import scala.language.reflectiveCalls
      |import scala.language.implicitConversions
      |
      |/**
      | * @author Sebastian Riedel
      | */
      |object MLEExample extends App {
      |
      | import WolfeEnv._
      |
      |  //training data
      |  val data = Seq('H, 'T, 'T, 'T)
      |
      |  //elements of the domain
      |  val coins = Set('H, 'T)
      |
      |  //log-likelihood objective
      |  @Objective.LogLikelihood
      |  def ll(data: Seq[Symbol])(prob: Symbol => Double) = sum(data) {x => log(prob(x))}
      |
      |  //the ML estimate
      |  val p = argmax(simplex(coins,Set(0.0, 0.2, 1.0))) {p => ll(data)(p)}
      |
      |  println(p('T))     |
      |}
    """.stripMargin

  def main(args: Array[String]) {
    val path = dirPathOfClass(getClass.getName)
    val compiler = new StringCompiler(additionalClassPath = List(path), transformer = Some(new WolfeTransformer {
      def transformTree[T <: Global#Tree](global: Global, tree: T) = {
        global.treeBrowser.browse(tree.asInstanceOf[global.Tree]); tree
      }
    }))
    compiler.compileCode(source)
  }

  object MLETransformer extends WolfeTransformer {

    def transformTree[T <: Global#Tree](global: Global, tree: T) = {
      tree match {
        case global.Ident(name) =>
      }
      tree
    }
  }

  private def dirPathOfClass(className: String) = try {
    val resource = className.split('.').mkString("/", "/", ".class")
    val path = getClass.getResource(resource).getPath
    val root = path.dropRight(resource.length)
    root
  }

}


object WolfeEnv {
  @Domain.Maps
  def maps[A, B](dom: Set[A], range: Set[B]): Set[Map[A,B]] = {
    def recurse(d: List[A], r: List[B], funs: List[Map[A,B]] = List(Map.empty)):List[Map[A,B]] = d match {
      case Nil => funs
      case head :: tail =>
        val newFunctions = for (value <- r; f <- funs.view) yield f + (head -> value)
        recurse(tail,r,newFunctions)
    }
    recurse(dom.toList,range.toList).toSet
  }

  def vectors[A, B](dom: Set[A], range: Set[B]): Set[Map[Any,B]] = {
    def recurse(d: List[A], r: List[B], funs: List[Map[Any,B]] = List(Map.empty)):List[Map[Any,B]] = d match {
      case Nil => funs
      case head :: tail =>
        val newFunctions = for (value <- r; f <- funs.view) yield f + (head -> value)
        recurse(tail,r,newFunctions)
    }
    recurse(dom.toList,range.toList).toSet
  }


  def seqs[A](dom: Set[A], length: Int): Set[Seq[A]] = ???

  @Operator.Sum
  def sum[T](elems: Seq[T])(f: T => Double) = elems.map(f).sum

  @Operator.Max
  def max[T](elems: Set[T])(f: T => Double) = elems.map(f).max

  @Operator.Argmax
  def argmax[T](dom: Set[T])(obj: T => Double): T = {
    dom.maxBy(obj)
  }

  @Operator.Argmin
  def argmin[T](dom: Set[T])(obj: T => Double): T = {
    dom.minBy(obj)
  }


  class All[T] extends Set[T] {
    def +(elem: T) = this
    def -(elem: T) = sys.error("Can't remove element from all objects")
    def contains(elem: T) = true
    def iterator = sys.error("Can't iterate over all objects")
  }

  @Domain.Simplex
  def simplex[T](domain: Set[T], range: Set[Double] = doubles) =
    for (p <- maps(domain,range); if sum(domain.toSeq) {p(_)} == 1.0 && domain.forall(p(_) >= 0.0)) yield p

  def expect[T](dom:Set[T],stats:T=>Vector)(obj:T=>Double) = dom.view.map(x => stats(x) * obj(x))

  @Objective.LogZ
  def logZ[T](dom:Set[T])(model:T=>Double) = math.log(dom.view.map(x => math.exp(model(x))).sum)

  def forall[T](dom: Set[T])(pred: T => Boolean) = dom.forall(pred)

  def all[T] = new All[T]

  val doubles: Set[Double] = new All[Double]

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

  type Vector = Map[Any,Double]

  val vectors = new All[Vector]

  def ft(key:Any,value:Double = 1.0) = Map(key -> value)

  implicit object VectorNumeric extends Numeric[Vector] {
    def plus(x: WolfeEnv.Vector, y: WolfeEnv.Vector) = {
        val keys = x.keySet ++ y.keySet
        val result =  keys map (k => k -> (x.getOrElse(k,0.0) + y.getOrElse(k, 0.0)))
        result.toMap
    }
    def minus(x: WolfeEnv.Vector, y: WolfeEnv.Vector) = ???
    def times(x: WolfeEnv.Vector, y: WolfeEnv.Vector) = ???
    def negate(x: WolfeEnv.Vector) = ???
    def fromInt(x: Int) = ???
    def toInt(x: WolfeEnv.Vector) = ???
    def toLong(x: WolfeEnv.Vector) = ???
    def toFloat(x: WolfeEnv.Vector) = ???
    def toDouble(x: WolfeEnv.Vector) = ???
    def compare(x: WolfeEnv.Vector, y: WolfeEnv.Vector) = ???
    def dot(x:Vector,y:Vector) = {
      x.keys.view.map(k => x(k) * y(k)).sum
    }
    override def zero = Map.empty
    def norm(x:Vector) = {
      val sum = x.values.sum
      x mapValues (_ / sum)
    }
  }

  implicit class RichVector(vector:Vector) {
    import WolfeEnv.{VectorNumeric => num}
    def +(that:Vector) = num.plus(vector,that)
    def dot(that:Vector) = num.dot(vector,that)
    def norm = num.norm(vector)
    def *(scale:Double) = vector.mapValues(_ * scale)
  }


}


import scala.annotation._


object Operator {

  class Argmax extends StaticAnnotation

  class Argmin extends StaticAnnotation

  class Sum extends StaticAnnotation

  class Max extends StaticAnnotation


}

object Objective {

  case class OptimizerSetting(algoritm:String)

  trait InferenceSetting

  trait GradientBasedOptimizerSetting

  case class Adagrad(rate:Double) extends GradientBasedOptimizerSetting
  class JointLoglikelihood extends StaticAnnotation

  case class MaxProduct(iterations:Int) extends InferenceSetting

  class LogLikelihood extends StaticAnnotation

  class Differentiable(setting:GradientBasedOptimizerSetting = Adagrad(1.0)) extends StaticAnnotation



  class LinearModel(setting:InferenceSetting = MaxProduct(1)) extends StaticAnnotation

  class Atomic extends StaticAnnotation

  class LogZ extends StaticAnnotation


}

object Domain {

  class PMF extends StaticAnnotation

  class Maps extends StaticAnnotation

  class Seqs extends StaticAnnotation

  class Simplex extends StaticAnnotation

  class Marginals extends StaticAnnotation

}


