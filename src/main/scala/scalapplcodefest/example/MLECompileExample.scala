package scalapplcodefest.example

import scalapplcodefest.compiler.{WolfeCompilerPlugin2, WolfeTransformer, StringCompiler}
import scala.language.implicitConversions
import scala.tools.nsc.Global
import scala.tools.nsc.interpreter.AbstractFileClassLoader

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
      |class MLEExampleWithLinearModelCompiled extends App {
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
      |  @Stats.Categorial
      |  def f(coin:Coin) = ft(coin)
      |
      |  @Objective.GLM
      |  def s(params: Vector)(coin: Coin) = f(coin) dot params
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
      |  val compiled = sum (data) {ft(_)} mapValues(w => log(w / data.size))
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

        def transformTree[T <: Global#Tree](global: Global, env: WolfeCompilerPlugin2#Environment, tree: T) = {
          val pat = new Extractors(global)
          import global._
          val printer = newRawTreePrinter()
          val result = tree match {
            case t@PackageDef(_, _) =>
//              global.treeBrowser.browse(t.asInstanceOf[global.Tree])
//              println(env.implementations)
              tree
            case pat.ApplyOperator("scalapplcodefest.Wolfe.Operator.Argmin", argmin, dom, obj) =>
              println("Yeah!")
              println(dom)
              println(obj)
              //now analyze the objective
              //todo: remove need for type cast
              obj.asInstanceOf[global.Tree] match {
                case global.Block(_, Function(valDef, body@Apply(Apply(fun, arg1), arg2))) =>
                  println(valDef)
                  println(body)
                  println(fun)
                  println(arg1)
                  println(arg2)
                  val sym = fun.symbol
                  val imps = env.implementations
//                  val hack = imps.find(_._1.rawname == sym.rawname).get._1
                  val imp = imps(fun.symbol).asInstanceOf[global.Tree]
                  imp match {
                    case pat.NumOpApply(sumOp@pat.OpName("scalapplcodefest.Wolfe.Operator.Sum"), data, perInstanceLoss, sumNum) =>
                      //                    case pat.ApplyOperator("scalapplcodefest.Wolfe.Operator.Sum", sum, data, perInstanceLoss) =>
                      println("data: " + data)
                      println(perInstanceLoss)
                      println(sumNum)
                      perInstanceLoss.asInstanceOf[global.Tree] match {
                        case Function(cDef, Apply(logZ, List(Apply(Apply(s, w), List(c))))) =>
                          println("Found model: " + s)
                          val sImp = imps.get(s.symbol).get
                          println(sImp)
                          sImp.asInstanceOf[global.Tree] match {
                            case Apply(Select(Apply(rich, List(vectorResult@Apply(feat, _))), dot), List(params)) =>
                              println("Got the feature function: " + feat)
                              //let's build the new term which is a sum


                              val sumData = data
                              val sumFunction = feat
                              val scalapplfestTerm = newTermName("scalapplcodefest")
                              //todo: ugly hack
                              val vectorType = vectorResult.tpe

                              println("VectorType: " + vectorType)

                              val test = global.rootMirror.getPackage(newTermName("scalapplcodefest"))
                              //val test2 = global.rootMirror.findMemberFromRoot("")
                              //global.rootMirror.sta
                              println(test)

                              val ident = Ident(scalapplfestTerm)
                              //todo: ugly hack
                              val scalapplcodefestSymbol = argmin.symbol.owner.owner.asInstanceOf[global.Symbol]

                              println(test == scalapplcodefestSymbol)


                              //                              val tpe1 = scalapplcodefestSymbol.tpe
//                              val tpe1Children = tpe1.members
//                              val symbol1 = tpe1.member(newTypeName("Wolfe"))
//                              val tpe2 = symbol1.tpe
//                              val tpe2Children = tpe2.members
//                              val vectorType2 = tpe2.member(newTermName("Vector")).tpe
//                              println(vectorType2)


                              ident.symbol = scalapplcodefestSymbol

                              val sumNum = Select(Select(ident, newTypeName("Wolfe")), newTypeName("VectorNumeric"))
                              val newSum = pat.NumOpApply(
                                sumOp.asInstanceOf[pat.global.Tree],
                                sumData.asInstanceOf[pat.global.Tree],
                                sumFunction.asInstanceOf[pat.global.Tree], sumNum.asInstanceOf[pat.global.Tree])

                              println(newSum)
                              printer.print(newSum)
                              println()
                              newSum


                            case _ => tree
                          }
                      }
                    case _ => tree
                  }
                //                  println(env.implementations.get(fun.symbol))
                //                  tree
                case _ =>
                  println("No idea how to transform " + obj)
                  tree
              }

            case _ =>  tree
          }
          result.asInstanceOf[T]
        }
      }))
    compiler.compileCode(source)
//    val classLoader = new AbstractFileClassLoader(compiler.outputDir, this.getClass.getClassLoader)
//    val cls = classLoader.loadClass("scalapplcodefest.example.MLEExampleWithLinearModelCompiled") // where className is the name of the class/object in the code
//    cls.newInstance()// this runs the code
  }


  class Extractors(val global: Global) {

    object ApplyOperator {

      import global._

      def unapply(tree: Any): Option[(String, global.Tree, global.Tree, global.Tree)] = tree match {
        case Apply(Apply(TypeApply(s@Select(_, name), _), List(dom)), List(obj))
          if s.symbol.annotations.exists(_.atp.toString().startsWith("scalapplcodefest.Wolfe.Operator")) =>
          val annotation = s.symbol.annotations.find(_.atp.toString().startsWith("scalapplcodefest.Wolfe.Operator")).get
          Some(annotation.atp.toString(), s, dom, obj)
        case Apply(Apply(Apply(TypeApply(s@Select(_, name), _), List(dom)), List(obj)), List(num))
          if s.symbol.annotations.exists(_.atp.toString().startsWith("scalapplcodefest.Wolfe.Operator")) =>
          val annotation = s.symbol.annotations.find(_.atp.toString().startsWith("scalapplcodefest.Wolfe.Operator")).get
          Some(annotation.atp.toString(), s, dom, obj)
        case _ => None
      }
    }

    object NumOpApply {

      import global._

      def unapply(tree: Any): Option[(global.Tree, global.Tree, global.Tree, global.Tree)] = tree match {
        case Apply(Apply(Apply(op@OpName(_), List(dom)), List(obj)), List(num)) =>
          Some(op, dom, obj, num)
        //Some(annotation.atp.toString(), s, dom, obj)
        case _ => None
      }
      def apply(op: global.Tree, dom: global.Tree, obj: global.Tree, num: global.Tree) = {
        Apply(Apply(Apply(op, List(num)), List(obj)), List(dom))
      }
    }

    object OpName {

      import global._

      def unapply(tree: Any) = tree match {
        case TypeApply(s@Select(_, name), _)
          if s.symbol.annotations.exists(_.atp.toString().startsWith("scalapplcodefest.Wolfe.Operator")) =>
          val annotation = s.symbol.annotations.find(_.atp.toString().startsWith("scalapplcodefest.Wolfe.Operator")).get
          Some(annotation.atp.toString())
      }
    }


  }

  object MLETransformer extends WolfeTransformer {

    def transformTree[T <: Global#Tree](global: Global, env: WolfeCompilerPlugin2#Environment, tree: T) = {
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






