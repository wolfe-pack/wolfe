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
    val compiler = new StringCompiler(additionalClassPath = List(path))
    val (global, unit) = compiler.compileCode(source)
    //
    val tree = unit.body.asInstanceOf[global.Tree]

    //    println(jarPathOfClass("scalapplcodefest.example.MLECompileExample"))
    global.treeBrowser.browse(tree)
    //    val path = dirPathOfClass(getClass.getName)
    //    println(path)
  }

  object MLETransformer extends WolfeTransformer {
    def transform(global: Global)(unit: global.CompilationUnit) = {
      unit.body match {
        case global.Ident(name) =>
      }
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
  @Domain.Functions
  def funs[A, B](dom: Set[A], range: Set[B]): Set[Map[A,B]] = {
    def recurse(d: List[A], r: List[B], funs: List[Map[A,B]] = List(Map.empty)):List[Map[A,B]] = d match {
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

  class All[T] extends Set[T] {
    def +(elem: T) = this
    def -(elem: T) = sys.error("Can't remove element from all objects")
    def contains(elem: T) = true
    def iterator = sys.error("Can't iterate over all objects")
  }

  @Domain.Simplex
  def simplex[T](domain: Set[T], range: Set[Double] = doubles) =
    for (p <- funs(domain,range); if sum(domain.toSeq) {p(_)} == 1.0 && domain.forall(p(_) >= 0.0)) yield p

  type Query[T, M] = (T => M, Set[M])

  def expect[K, M, T](dom: Set[T], queries: Map[K, Query[T, M]])(obj: T => Double): Map[K, M => Double] = {
    type Marg = Seq[M => T]
    ???
  }

  def forall[T](dom: Set[T])(pred: T => Boolean) = dom.forall(pred)

  def all[T] = new All[T]

  val doubles: Set[Double] = new All[Double]

  val strings: Set[String] = new All[String]

  case class RichSet[T](set: Set[T]) {
    def ->[B](that: Set[B]) = funs(set, that)
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

  implicit class RichVector(vector:Vector) {
    def dot(that:Vector) = {
      vector.keys.map(k => vector(k) * that(k)).sum
    }
    def +(that:Vector) = {
      val keys = vector.keySet ++ that.keySet
      val result =  keys map (k => k -> (vector.getOrElse(k,0.0) + that.getOrElse(k, 0.0)))
      result.toMap
    }
  }


}


import scala.annotation._


object Operator {

  class Argmax extends StaticAnnotation

  class Sum extends StaticAnnotation

  class Max extends StaticAnnotation


}

object Objective {

  class LogLikelihood extends StaticAnnotation

  class MaxProduct(iterations: Int) extends StaticAnnotation

}

object Domain {

  class PMF extends StaticAnnotation

  class Functions extends StaticAnnotation

  class Seqs extends StaticAnnotation

  class Simplex extends StaticAnnotation

  class Marginals extends StaticAnnotation

}


