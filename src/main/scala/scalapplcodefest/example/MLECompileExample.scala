package scalapplcodefest.example

import scalapplcodefest.compiler.{WolfeCompilerPlugin2, WolfeTransformer, StringCompiler}
import scala.language.implicitConversions
import scala.tools.nsc.Global
import scala.reflect.internal.Symbols
import scalapplcodefest.Wolfe

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

        def transformTree[T <: Global#Tree](global: Global,env:WolfeCompilerPlugin2#Environment, tree: T) = {
          val pat = new Extractors(global)
          import global._
          tree match {
            case t @ PackageDef(_,_) =>
              //global.treeBrowser.browse(t.asInstanceOf[global.Tree])
              println(env.implementations)
            case i @ Ident(name) =>
//              println(i)
            case valdef @ global.ValDef(_,_,_,app@Apply(f,_)) =>
//              println(f.getClass)
            case pat.ApplyOperator("scalapplcodefest.Wolfe.Operator.Argmin",op,dom,obj) =>
              println("Yeah!")
              println(dom)
              println(obj)
              //now analyze the objective
              //todo: remove need for type cast
              obj.asInstanceOf[global.Tree] match {
                case global.Block(_,Function(valDef,body@Apply(Apply(fun,arg1),arg2))) =>
                  println(valDef)
                  println(body)
                  println(fun)
                  println(arg1)
                  println(arg2)
                  val sym = fun.symbol
                  val imps = env.implementations
                  val hack = imps.find(_._1.rawname == sym.rawname).get._1
                  println(env.implementations.get(fun.symbol))
                case _ => println("No idea how to transform " + obj)
              }
            case app @ Apply(Apply(TypeApply(s@Select(_,name),_),dom),obj) =>
              println(name)
              println(s.symbol)
              println(s.symbol.annotations)
              println(env.implementations.get(s.symbol))
              for (annotation <- s.symbol.annotations) {
                if (annotation.atp.toString() == "scalapplcodefest.Wolfe.Operator.Argmin") {
                  println("Translating Argmin...")
                  println(dom)
                  println(obj)
                }
                //if (annotation.symbol.name)
              }
//              println("Fun: " + f)
//              println("Arg: " + a)
//              println(name.encoded)
//              println(name2.encoded)
            case s @ Select(_,name) if name.encoded == "argmin" =>
              val symbol = s.symbol
//              println(symbol)
              val sig = symbol.owner.typeSignature.members
              val members = symbol.owner.tpe.members
              val children = symbol.owner.children
//              println(members)
//              println(env.implementations)
              val map = env.implementations
//              println(env.implementations.get(s.symbol))

            case _ =>
          }
          tree
        }
      }))
    compiler.compileCode(source)
  }



  class Extractors(val global:Global) {

    object ApplyOperator {
      import global._
      def unapply(tree:global.Tree):Option[(String,global.Tree,global.Tree,global.Tree)] = tree match {
        case Apply(Apply(TypeApply(s@Select(_,name),_),List(dom)),List(obj))
          if s.symbol.annotations.exists(_.atp.toString().startsWith("scalapplcodefest.Wolfe.Operator")) =>
          val annotation = s.symbol.annotations.find(_.atp.toString().startsWith("scalapplcodefest.Wolfe.Operator")).get
          Some(annotation.atp.toString(),s,dom,obj)
        case _=> None
      }
    }
  }

  object MLETransformer extends WolfeTransformer {

    def transformTree[T <: Global#Tree](global: Global,env:WolfeCompilerPlugin2#Environment, tree: T) = {
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






