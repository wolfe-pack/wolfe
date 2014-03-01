package scalapplcodefest.legacy.example

import scalapplcodefest.legacy.compiler.{TransformerUtilities, WolfeCompilerPlugin2, WolfeTransformer, StringCompiler}
import scala.language.implicitConversions
import scala.tools.nsc.Global
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.reflect.internal.Flags

/**
 * @author Sebastian Riedel
 */
object MLECompileExample {

  val source =
    """
      |package scalapplcodefest.legacy.example
      |
      |import math.log
      |import scalapplcodefest._
      |import scala.language.reflectiveCalls
      |import scala.language.implicitConversions
      |
      |/**
      | * @author Sebastian Riedel
      | */
      |class MLEExampleWithLinearModelCompiled extends (() => Any) {
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
      |  val compiled2 = sum (data) {f}
      |
      |  println(compiled)
      |  println(w)
      |
      |  def fun = {
      |     val x = 5
      |     x
      |  }
      |  println("Hallo")
      |
      |  def apply() = {
      |    w
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
          import typer._
          val printer = newRawTreePrinter()
          val result = tree match {
            case t@PackageDef(_, _) =>
//              global.treeBrowser.browse(t.asInstanceOf[global.Tree])
              //              println(env.implementations)
              tree
            case outerSum@pat.NumOpApply(sumOp@pat.OpName("scalapplcodefest.Wolfe.Operator.Sum", _, _), _, _, _) =>
              println("A sum: " + outerSum)
              tree
            case argminTree@pat.ApplyOperator("scalapplcodefest.Wolfe.Operator.Argmin", argmin, dom, obj, List(argminVectorType)) =>
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
                    case outerSum@pat.NumOpApply(sumOp@pat.OpName("scalapplcodefest.Wolfe.Operator.Sum", typeArgs@List(dataType, doubleType), existingSumSelect), data, perInstanceLoss, sumNum) =>
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

                              abort("Blah!")

                              import global.rootMirror._

                              val sumData = data
                              val sumFunction = feat
                              val sumFunctionBlock = {
                                val valueTpe = dataType.tpe.asInstanceOf[global.Type]
                                val symbolForOwner = tree.asInstanceOf[global.Tree].symbol
                                val anomParam = symbolForOwner.owner.newValue(newTermName("tmp"))
                                  .setFlag(Flags.SYNTHETIC)
                                  .setInfo(valueTpe).asInstanceOf[global.Symbol]
                                val apply = Apply(sumFunction, List(Ident(anomParam)))
                                val paramDef = ValDef(anomParam)
                                val function = Function(List(paramDef), apply)
                                typed(function)
                                //namer.ass
                                //namer.assignSymbol()
                                Block(Nil, function)
                              }

                              val wolfeSym = getModuleByName(newTermName("scalapplcodefest.Wolfe"))
                              val sumSym = global.definitions.getMember(wolfeSym, newTermName("sum"))
                              val vectorNumSym = global.definitions.getMemberModule(wolfeSym, newTermName("VectorNumeric"))
                              val vectorNum = Ident(vectorNumSym)

                              // getClassByName(newTermName("scalapplcodefest.Wolfe.Vector"))
                              val vectorType = TransformerUtilities.getClassTypeByName(global, "scalapplcodefest.Wolfe.Vector")
                              //val vectorTypeTerm = Select(Select(Ident(newTermName("scalapplcodefest")),newTermName("wolfe")),newTypeName("Vector"))
                              val vectorTypeTerm = Select(Select(Ident(newTermName("scalapplcodefest")), newTermName("Wolfe")), newTypeName("Vector"))
                              val vectorTypeTerm2 = Select(Ident(newTermName("scalapplcodefest.Wolfe")), newTypeName("Vector"))

                              //val sumSelect2 = typed(Select(Select(Ident("scalapplcodefest"), "Wolfe"),"sum"))
                              val sumSelect = Select(Ident(wolfeSym), "sum")
                              //sumOp.tpe = null
                              val oldSumOp = sumOp //typed(sumOp.asInstanceOf[global.Tree])
                            val sumDataVectorType = TypeTree(vectorType.asInstanceOf[global.Type])
                              val untyped = TypeApply(existingSumSelect.asInstanceOf[global.Tree], List(dataType.asInstanceOf[global.Tree], argminVectorType.asInstanceOf[global.Tree]))
                              //                              val untyped = TypeApply(sumSelect, typeArgs.asInstanceOf[List[global.Tree]])
                              //val sumDataVector = typed(TypeApply(sumSelect, List(argminVectorType.asInstanceOf[global.Tree],vectorTypeTerm))) //Argmin Domain, Vector
                              val sumDataVector = untyped // typed(untyped) //Argmin Domain, Vector
                            val blah = outerSum

                              val test = reify(1 + 2)
                              println(test)

                              println(sumDataVectorType.tpe.isErroneous)
                              println(blah)


                              val afterData = global.Apply(sumDataVector, List(sumData.asInstanceOf[global.Tree]))
                              val afterObj = global.Apply(afterData, List(sumFunction))
                              val afterDom = Apply(afterObj, List(vectorNum))

                              global.resetAllAttrs(afterDom)

                              //val typedSumFunction = typed(sumFunction)

                              val typerContext = typer.context
                              val analyzerContext = analyzer.rootContext(global.currentUnit,tree.asInstanceOf[global.Tree])


                              val localTyper = analyzer.newTyper(analyzerContext)

                              //namer.
                              val typedResult: global.Tree = localTyper.typed(atPos(argminTree.pos)(afterDom))

                              val untypedZero = Select(Ident(wolfeSym), newTermName("VectorZero"))
                              //val vectorZeroSym = global.definitions
                              val vectorZero = typed(untypedZero)
                              typed(vectorZero)

                              global.abort()

                              val myOuterSum = outerSum
                              val typedTree = typed(outerSum)

                              println(outerSum)
                              println(afterDom)

                              val nullTpe = typedResult.filter(t => t.symbol != null && t.symbol.owner == null)

//                              vectorZero
                              typedResult
                            //                              tree
                            //                              typed(afterDom)


                            //val sumForVector = typer.typed(TypeApply(Select()))


                            //                              val scalapplfestTerm = newTermName("scalapplcodefest")
                            //                              //todo: ugly hack
                            //                              val vectorType = vectorResult.tpe
                            //
                            //                              println("VectorType: " + vectorType)
                            //
                            //                              val test = global.rootMirror.getPackage(newTermName("scalapplcodefest"))
                            //                              //val test2 = global.rootMirror.findMemberFromRoot("")
                            //                              //global.rootMirror.sta
                            //                              println(test)
                            //
                            //
                            //                              val ident = Ident(scalapplfestTerm)
                            //                              //todo: ugly hack
                            //                              val scalapplcodefestSymbol = argmin.symbol.owner.owner.asInstanceOf[global.Symbol]
                            //
                            //                              println(test == scalapplcodefestSymbol)
                            //
                            //
                            //                              //                              val tpe1 = scalapplcodefestSymbol.tpe
                            ////                              val tpe1Children = tpe1.members
                            ////                              val symbol1 = tpe1.member(newTypeName("Wolfe"))
                            ////                              val tpe2 = symbol1.tpe
                            ////                              val tpe2Children = tpe2.members
                            ////                              val vectorType2 = tpe2.member(newTermName("Vector")).tpe
                            ////                              println(vectorType2)
                            //
                            //
                            //                              ident.symbol = scalapplcodefestSymbol
                            //
                            //                              val sumNum = Select(Select(ident, newTypeName("Wolfe")), newTypeName("VectorNumeric"))
                            //                              val newSum = pat.NumOpApply(
                            //                                sumOp.asInstanceOf[pat.global.Tree],
                            //                                sumData.asInstanceOf[pat.global.Tree],
                            //                                sumFunction.asInstanceOf[pat.global.Tree], sumNum.asInstanceOf[pat.global.Tree])
                            //
                            //
                            //                              val typedSum = global.typer.typed(newSum.asInstanceOf[global.Tree])
                            //
                            //                              println(newSum)
                            //                              printer.print(newSum)
                            //                              println()
                            //                              newSum


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

            case _ => tree
          }
          result.asInstanceOf[T]
        }
      }))
    compiler.compileCode(source)
    val classLoader = new AbstractFileClassLoader(compiler.outputDir, this.getClass.getClassLoader)
    val cls = classLoader.loadClass("scalapplcodefest.legacy.example.MLEExampleWithLinearModelCompiled") // where className is the name of the class/object in the code
    val obj = cls.newInstance().asInstanceOf[() => Any] // this runs the code
    val otherClass = getClass
    println(obj)
    println(obj.apply())
  }


  class Extractors(val global: Global) {

    object ApplyOperator {

      import global._

      def unapply(tree: Any): Option[(String, global.Tree, global.Tree, global.Tree, List[global.Tree])] = tree match {
        case Apply(Apply(TypeApply(s@Select(_, name), types), List(dom)), List(obj))
          if s.symbol.annotations.exists(_.atp.toString().startsWith("scalapplcodefest.Wolfe.Operator")) =>
          val annotation = s.symbol.annotations.find(_.atp.toString().startsWith("scalapplcodefest.Wolfe.Operator")).get
          Some(annotation.atp.toString(), s, dom, obj, types)
        case Apply(Apply(Apply(TypeApply(s@Select(_, name), types), List(dom)), List(obj)), List(num))
          if s.symbol.annotations.exists(_.atp.toString().startsWith("scalapplcodefest.Wolfe.Operator")) =>
          val annotation = s.symbol.annotations.find(_.atp.toString().startsWith("scalapplcodefest.Wolfe.Operator")).get
          Some(annotation.atp.toString(), s, dom, obj, types)
        case _ => None
      }
    }

    object NumOpApply {

      import global._

      def unapply(tree: Any) = tree match {
        case Apply(Apply(Apply(op@OpName(_, types, _), List(dom)), List(obj)), List(num)) =>
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
        case TypeApply(s@Select(_, name), types)
          if s.symbol.annotations.exists(_.atp.toString().startsWith("scalapplcodefest.Wolfe.Operator")) =>
          val annotation = s.symbol.annotations.find(_.atp.toString().startsWith("scalapplcodefest.Wolfe.Operator")).get
          Some(annotation.atp.toString(), types, s)
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






