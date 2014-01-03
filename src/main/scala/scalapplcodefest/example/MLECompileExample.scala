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
      |  import WolfeEnv._
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
      |  @Domain.PMF
      |  def distributions = for (p <- coins -> doubles; if sum(coins) {p(_)} == 1.0 && coins.forall(p(_) >= 0.0)) yield p
      |
      |  //the ML estimate
      |  val p = argmax (distributions) {ll(data)(_)}
      |
      |  println(p('T))
      |
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

  private def dirPathOfClass(className:String) = try {
    val resource = className.split('.').mkString("/", "/", ".class")
    val path = getClass.getResource(resource).getPath
    val root = path.dropRight(resource.length)
    root
  }

}



object WolfeEnv {
  def funs[A, B](dom: Set[A], range: Set[B]): Set[A => B] = ???
  def seqs[A](dom:Set[A],length:Int):Set[Seq[A]] = ???

  @Operator.Sum
  def sum[T](elems: Seq[T])(f: T => Double) = elems.map(f).sum

  @Operator.Max
  def max[T](elems: Set[T])(f: T => Double) = elems.map(f).max

  @Operator.Argmax
  def argmax[T](dom: Set[T])(obj: T => Double): T = {
    dom.maxBy(obj)
  }

  type Query[T,M] = (T=>M,Set[M])

  def expect[K,M,T](dom:Set[T],queries:Map[K,Query[T,M]])(obj: T => Double): Map[K,M=>Double] = {
    type Marg = Seq[M => T]
    ???
  }

  def forall[T](dom: Set[T])(pred: T => Boolean) = dom.forall(pred)

  val doubles: Set[Double] = Iterator.continually(math.random).toSet

  val strings: Set[String] = Iterator.continually(math.random.toString).toSet

  case class RichSet[T](set:Set[T]) {
    def ->[B](that: Set[B]) = funs(set, that)
  }
  implicit def toRichSet[T](set: Set[T]) = RichSet(set)

  implicit def toSeq[T](seq: Set[T]) = seq.toSeq

  case class RichCurried[A1,A2,B](f: A1 => A2 => B) {
    def apply(pair:(A2,A1)) = f(pair._2)(pair._1)
  }
  implicit def toRichCurried[A1,A2,B](f: A1 => A2 => B)= new RichCurried(f)

  implicit class BarBuilder[T](t:T) {
    def |[A](that:A) = t -> that
  }




}