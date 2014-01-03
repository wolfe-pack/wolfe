package scalapplcodefest.compiler

/**
 *
 *
 * @author svivek
 */
object ASTExplorer extends App {
  def print(code: String) = {

    val wrappedCode = "object TestObject { " + code + "}"

    val (global, unit) = StringCompiler.compileCode(wrappedCode)

    val tree = unit.body.asInstanceOf[global.Tree]

    // ignore the package, the object and the first element of the object, which is always the constructor
    val codeAST = tree.asInstanceOf[global.PackageDef].stats(0).asInstanceOf[global.ModuleDef].impl.body.tail

    codeAST.foreach {c => global.newRawTreePrinter().print(c); println()}

  }

  val types =
    """
      |type Foo1 = Int
      |type Foo2 = String
      |type Foo3 = Set[String]
      |type Foo4 = Map[String, Int]
      |type Foo5 = (String, Int)
      |type Foo6 = scala.util.Random
      |type Foo7 = (String, Int, Double)
      |type Foo8 = Foo
      |type Foo9 = String => Double
      |type Foo10 = (String, Int, Boolean) => Double
      |type Foo11 = String => Int => Double
      |type Foo12[T] = Set[T]
      |type Foo13[T, S] = Map[T, S]
    """.stripMargin

  print(types)


  val objects =
    """
      |1
      |"wolfe"
      |1.3
      |true
      |(1, 2)
      |new Foo
      |new Foo(a, b)
      |new some.where.Foo
      |new some.where.Foo(a, b)
      |'Anna
    """.stripMargin

  print(objects)


  val vals =
    """
      |val num = 1
      |private val dbl: Double = 1.3
      |val foo = new Foo
      |val fooab = new Foo(a, b)
    """.stripMargin

  print(vals)

  val functionCalls =
    """
      |val a = f(10)
      |val b = foo.call(1, 3)
      |bar.execute("mary", "lamb")
      |1 + 3
      |a + 3
      |f1 + f2
      |(f1 + f2) dot weights
      |x :: xs
    """.stripMargin

  print(functionCalls)

  val functionDefs =
    """
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
    """.stripMargin

  print(functionDefs)

  val lambdas =
    """
      |val f = {x: Int => x}
      |val g = {p: (Int, Int) => p._1 + p._2}
      |val h = {x: Int => y: Int => x + y}
    """.stripMargin

  print(lambdas)

  val classes =
    """
      |class A { val a = 1; def f = 20 }
      |class A(val a: Int, b: Double)
      |class A(val a: Int = 1, b: Double)
      |case class A (a: Int, b: Double)
      |case class A(a: Int = 3, b: Double = 4.0)
      |object A { val a = 1; def f = 20 }
    """.stripMargin

  print(classes)

  val block =
    """
      |{ val a = 10; def b = a + 1 }
    """.stripMargin

  print(block)

  val forComprehension =
    """
      |for(i <- 0 until 10) { println(i)}
      |for(i <- 0 until 10) { i }
      |for(i <- 0 until 10) yield i
      |for(i <- 0 until 10; j <- i until 10) yield i + j
      |for(i <- 0 until 10; j <- 0 until 10; if i >=j) yield i + j
    """.stripMargin

  print(forComprehension)

}
