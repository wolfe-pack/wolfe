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
      |import scalapplcodefest._
      |import scala.language.reflectiveCalls
      |import scala.language.implicitConversions
      |
      |/**
      | * @author Sebastian Riedel
      | */
      |object MLEExampleWithLinearModel extends App {
      |
      |  import Wolfe._
      |
      |  type Coin = Symbol
      |
      |  //elements of the domain
      |  val coins = Set('H, 'T)
      |
      |  //training data
      |  val data = Seq('H, 'T, 'T, 'T)
      |
      |  @Objective.Categorical
      |  def s(params: Vector)(coin: Coin) = ft(coin) dot params
      |
      |  @Objective.JointLoglikelihood
      |  def ll(data: Seq[Coin])(w: Vector) = sum(data) {c => logZ(coins)(s(w)) - s(w)(c)}
      |
      |  //a subset of possible weights to make brute force search tractable
      |  val myReals = Set(0.0, 0.25, 0.75, 1.0).map(math.exp)
      |
      |  //val w = argmin(vectors)(ll(data)) //this won't run without compilation
      |  val w = argmin(vectors(coins, myReals)) {ll(data)} //this should run w/o compilation
      |
      |  //this is how the compiled expression should look like
      |  val compiled = (data map (ft(_))).sum.mapValues(w => log(w / data.size))
      |
      |  println(compiled)
      |  println(w)
      |
      |  def fun = {
      |     val x = 5
      |     x
      |  }
      |
      |}
      |    """.stripMargin

  def main(args: Array[String]) {
    val path = dirPathOfClass(getClass.getName)
    var browsed = false
    val compiler = new StringCompiler(
      additionalClassPath = List(path),
      runsAfter = List("typer"),
      transformer = Some(new WolfeTransformer {

        def transformTree[T <: Global#Tree](global: Global, tree: T) = {
          import global._
          tree match {
            case t @ PackageDef(_,_) =>
              global.treeBrowser.browse(t.asInstanceOf[global.Tree])
            case i @ Ident(name) =>
              println(i)
            case s @ Select(_,name) if name.encoded == "argmin" =>
              val symbol = s.symbol
              println(symbol)
              val sig = symbol.owner.typeSignature.members
              val members = symbol.owner.tpe.members
              val children = symbol.owner.children
              println(members)
            case _ =>
          }
          tree
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






