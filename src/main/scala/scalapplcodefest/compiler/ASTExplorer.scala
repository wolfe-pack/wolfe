package scalapplcodefest.compiler

import scala.tools.nsc.Global


/**
 *
 *
 * @author svivek
 */


object ASTExplorer extends App {

  /*
               phase name  id  description
             ----------  --  -----------
                 parser   1  parse source into ASTs, perform simple desugaring
                  namer   2  resolve names, attach symbols to named trees
         packageobjects   3  load package objects
                  typer   4  the meat and potatoes: type the trees
                 patmat   5  translate match expressions
         superaccessors   6  add super accessors in traits and nested classes
             extmethods   7  add extension methods for inline classes
                pickler   8  serialize symbol tables
              refchecks   9  reference/override checking, translate nested objects
           selectiveanf  10
           selectivecps  11
                uncurry  12  uncurry, translate function values to anonymous classes
              tailcalls  13  replace tail calls by jumps
             specialize  14  @specialized-driven class and method specialization
          explicitouter  15  this refs to outer pointers, translate patterns
                erasure  16  erase types, add interfaces for traits
            posterasure  17  clean up erased inline classes
               lazyvals  18  allocate bitmaps, translate lazy vals into lazified defs
             lambdalift  19  move nested functions to top level
           constructors  20  move field definitions into constructors
                flatten  21  eliminate inner classes
                  mixin  22  mixin composition
                cleanup  23  platform-specific cleanups, generate reflective calls
                  icode  24  generate portable intermediate code
                inliner  25  optimization: do inlining
inlineExceptionHandlers  26  optimization: inline exception handlers
               closelim  27  optimization: eliminate uncalled closures
                    dce  28  optimization: eliminate dead code
                    jvm  29  generate JVM bytecode
               terminal  30  The last phase in the compiler chain

   */


  def exploreAST(code: String, runsAfter: List[String], showBrowser: Boolean = false) = {

    val wrappedCode = "object TestObject { " + code + "}"

    val compiler = new StringCompiler(transformer = Some(new WolfeTransformer {

      def transformTree[T <: Global#Tree](global: Global, tree: T) = {
        // ignore the package, the object and the first element of the object, which is always the constructor
        tree match {
          case p: global.PackageDef => {

            val codeAST: List[global.Tree] = p.stats(0) match {
              case c: global.ClassDef => c.impl.body.tail
              case m: global.ModuleDef => m.impl.body.tail
              case x => List(x)
            }
            if (showBrowser)
              global.treeBrowser.browse(tree.asInstanceOf[global.Tree])


            codeAST.foreach {
              c => {
                global.newRawTreePrinter().withTypes.print(c)
                println()
              }
            }
          }
          case _ =>
        }


        tree

      }
    }), runsAfter = runsAfter)
    compiler.compileCode(wrappedCode)
  }

  val compilerPhases = List("parser", "namer", "packageobjects", "typer", "patmat", "superaccessors",
    "extmethods", "pickler", "refchecks", "selectiveanf", "selectivecps", "uncurry", "tailcalls",
    "specialize", "explicitouter", "erasure", "posterasure", "lazyvals", "lambdalift",
    "constructors", "flatten", "mixin", "cleanup", "icode", "inliner",
    "inlineExceptionHandlers", "closelim", "dce")
  // we will not look at the last two phases
  //"jvm", "terminal")

  def trackTransformations(code: String) = {
    compilerPhases.foreach {
      phase
      => {
        println("Phase: " + phase)
        exploreAST(code, List(phase), showBrowser = true)

        println()
      }
    }
  }

  def explore(codeExamples: collection.mutable.Map[String, List[String]],
              name: String,
              runsAfter: List[String],
              showTrees: Boolean = false) = {
    println(name)
    codeExamples(name).foreach {
      c => {
        print("| `" + c + "` | <code>")
        exploreAST(c, runsAfter, showTrees)
        println("`</code>")
      }
    }
  }

  val code = collection.mutable.Map[String, List[String]]()

  def register(name: String, lines: String) = code(name) = lines.trim.split("\\\n").toList

  val types = "Types"
  register(types, """
                    |type Foo1 = Int
                    |type Foo2 = String
                    |type Foo3 = Set[String]
                    |type Foo4 = Map[String, Int]
                    |type Foo5 = (String, Int)
                    |type Foo6 = scala.util.Random
                    |type Foo7 = (String, Int, Double)
                    |type Foo9 = String => Double
                    |type Foo10 = (String, Int, Boolean) => Double
                    |type Foo11 = String => Int => Double
                    |type Foo12[T] = Set[T]
                    |type Foo13[T, S] = Map[T, S]
                  """.stripMargin)

  val objects = "Objects"
  register(objects, """
                      |1
                      |"wolfe"
                      |1.3
                      |true
                      |(1, 2)
                      |new String
                      |new Set(1, 2)
                      |new scala.util.Random()
                      |new scala.util.Random(3)
                      |new HashSet[String]()
                      |'Anna
                    """.stripMargin)


  val vals = "Vals"
  register(vals, """
                   |val num = 1
                   |private val dbl: Double = 1.3
                   |val foo = new Foo
                   |val fooab = new Foo(a, b)
                 """.stripMargin)

  val functionCalls = "Function calls"
  register(functionCalls, """
                            |val a = f(10)
                            |val b = foo.call(1, 3)
                            |bar.execute("mary", "lamb")
                            |1 + 3
                            |a + 3
                            |f1 + f2
                            |(f1 + f2) dot weights
                            |x :: xs
                          """.stripMargin)

  val functionDefs = "Function definitions"
  register(functionDefs, """
                           |def f = 10
                           |def f: Int = 10
                           |def f(a: Int)  = a + 3
                           |def f[T](a: T) = a.toString
                           |def f[T, S](a: T) = createS(a)
                           |def f(a: Int = 2) = a + 3
                           |def f(a: Int = 2): Int = a + 3
                           |def f(a: Int) = { a + 3 }
                           |def f(a: Int) = { println(a); a + 3 }
                           |def f(a: Int, b: String) = a + b
                           |def f(a: Int)(b: String) = a + b
                         """.stripMargin)


  val lambdas = "Lambdas"
  register(lambdas, """
                      |val f = {x: Int => x}
                      |val g = {p: (Int, Int) => p._1 + p._2}
                      |val h = {x: Int => y: Int => x + y}
                    """.stripMargin)


  val classes = "Classes and objects"
  register(classes, """
                      |class A { val a = 1; def f = 20 }
                      |class A(val a: Int, b: Double)
                      |class A(val a: Int = 1, b: Double)
                      |case class A (a: Int, b: Double)
                      |case class A(a: Int = 3, b: Double = 4.0)
                      |object A { val a = 1; def f = 20 }
                    """.stripMargin)

  val block = "Code blocks"
  register(block, """
                    |{ val a = 10; def b = a + 1 }
                  """.stripMargin)

  val forComprehension = "For comprehensions"
  register(forComprehension, """
                               |for(i <- 0 until 10) { println(i)}
                               |for(i <- 0 until 10) { i }
                               |for(i <- 0 until 10) yield i
                               |for(i <- 0 until 10; j <- i until 10) yield i + j
                               |for(i <- 0 until 10; j <- 0 until 10; if i >=j) yield i + j
                             """.stripMargin)

  val annotations = "Annotations"
  register(annotations, """
                          |@deprecated def f(a: Int) = a + 1
                        """.stripMargin)

  //  explore(code, List("refchecks") objects)

  trackTransformations(
    """
      |val x = 1
      |val y = 2
      |val z = x + y
      |
      |def f(x: Int, y: Double) = z
      |
      |val z1 = f(1, 3.1)
      |
    """.stripMargin)

}
