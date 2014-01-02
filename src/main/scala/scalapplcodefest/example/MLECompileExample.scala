package scalapplcodefest.example

import scalapplcodefest.compiler.StringCompiler

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
      |object MLEExample {
      |
      |  def funs[A, B](dom: Set[A], range: Set[B]): Set[A => B] = ???
      |
      |  def sum[T](elems: Seq[T])(f: T => Double) = elems.map(f).sum
      |
      |  def max[T](elems: Set[T])(f: T => Double) = elems.map(f).max
      |
      |  def argmax[T](dom: Set[T])(obj: T => Double): T = {
      |    dom.maxBy(obj)
      |  }
      |
      |  val doubles: Set[Double] = Iterator.continually(math.random).toSet
      |  implicit def toRichSet[T](set: Set[T]) = new AnyRef {
      |    def ->[B](that: Set[B]) = funs(set, that)
      |  }
      |
      |  implicit def toSeq[T](seq: Set[T]) = seq.toSeq
      |
      |  def main(args: Array[String]) {
      |
      |    //training data
      |    val data = Seq('H, 'T, 'T, 'T)
      |
      |    //elements of the domain
      |    val coins = Set('H, 'T)
      |
      |    //log-likelihood objective
      |    @Objective.LogLikelihood
      |    def ll(data: Seq[Symbol])(prob: Symbol => Double) = sum(data) {x => log(prob(x))}
      |
      |    @Domain.PMF
      |    def distributions = for (p <- coins -> doubles; if sum(coins) {p(_)} == 1.0 && coins.forall(p(_) >= 0.0)) yield p
      |
      |    //the ML estimate
      |    val p = argmax (distributions) {ll(data)(_)}
      |
      |    println(p('T))
      |
      |  }
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

  private def dirPathOfClass(className:String) = try {
    val resource = className.split('.').mkString("/", "/", ".class")
    val path = getClass.getResource(resource).getPath
    val root = path.dropRight(resource.length)
    root
  }

  private def jarPathOfClass(className: String) = try {
    val resource = className.split('.').mkString("/", "/", ".class")
    val path = getClass.getResource(resource).getPath
    val indexOfFile = path.indexOf("file:") + 5
    val indexOfSeparator = path.lastIndexOf('!')
    List(path.substring(indexOfFile, indexOfSeparator))
  }

}
