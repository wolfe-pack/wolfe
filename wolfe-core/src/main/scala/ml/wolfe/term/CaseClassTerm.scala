package ml.wolfe.term

import ml.wolfe.Language
import ml.wolfe.term._

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
 * @author riedel
 */
object CaseClassTerm {

  import BaseEval._
  import Language._

  def main(args: Array[String]) {
    case class Test[C](value: C, number: Double)
    object Test {

      def construct[C](args: Seq[Any]) = args match {
        case Seq(v, n) => new Test(v.asInstanceOf[C], n.asInstanceOf[Double])
      }

      object TestTerm {

        def apply[C](value: Term[C], number: Term[Double]) =
          ConstructProduct(Seq(value, number), construct[C])

        def unapply[C](term: Term[Test[C]]) =
          Some(GetElement[C](term, 0), GetElement[Double](term, 1))

      }

      implicit class RichTerm[C](val t: Term[Test[C]]) {
        def value = GetElement[C](t, 0)

        def number = GetElement[Double](t, 1)
      }

      def Values[T](value: Dom[T], number: Dom[Double]) =
        ProductDom[Test[T]](Seq(value, number), construct[T])

    }

    import Test._

    def test[C](t: Term[Test[C]]): Term[Test[C]] = Test.TestTerm(t.value, t.number + 2.0)


    val t = Variable[Test[Int]]("t")
    val i = Variable[Int]("i")
    val d = Variable[Double]("d")
    val c = Test.TestTerm[Int](i, d)

    println(eval(t := Test(1, 2.0))(t.value))
    println(eval(i := 1, d := 2.0)(c.value))

  }

  def implOnClass(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    annottees.map(_.tree).toList match {
      case (caseClassDef: ClassDef) :: _ =>

        val caseClassTermName = caseClassDef.name.toTermName
        val caseClassTypeName = caseClassDef.name.toTypeName
        val q"case class ${_}[..${genTypes: List[TypeDef]}](..${caseClassArgs: List[ValDef]})" = caseClassDef
        val genTypenames = for (t <- genTypes) yield tq"${t.name}"
        val caseClassArgNames = caseClassArgs.map(_.name)
        val caseClassArgTypeNames: List[Tree] = caseClassArgs.map { case q"${_} val ${_}:$typ = ${_}" => typ }


        val getters = for ((argType, index) <- caseClassArgTypeNames.zipWithIndex) yield {
          q"ml.wolfe.term.GetElement[$argType](term,${Literal(Constant(index))})"
        }

        val getterMethods = for ((argName,getter) <- caseClassArgNames zip getters) yield {
          q"def $argName = $getter"
        }
        val castArgs = for ((argType,index) <- caseClassArgTypeNames.zipWithIndex) yield {
          q"args(${Literal(Constant(index))}).asInstanceOf[$argType]"
        }
        val termArgs = for (((argName,argType), index) <- (caseClassArgNames zip caseClassArgTypeNames).zipWithIndex) yield {
          q"val $argName:ml.wolfe.term.Term[$argType]"
        }
        val domArgs = for (((argName,argType), index) <- (caseClassArgNames zip caseClassArgTypeNames).zipWithIndex) yield {
          q"val $argName:ml.wolfe.term.Dom[$argType]"
        }

        val newTerm = q"""
          $caseClassDef
          object $caseClassTermName {

            def construct[..$genTypes](args:Seq[Any]) = new $caseClassTypeName(..$castArgs)
            implicit class RichTerm[..$genTypes](val term:ml.wolfe.term.Term[$caseClassTypeName[..$genTypenames]]) {
              ..$getterMethods
              def pimped:this.type = this
            }
            object Term {
              def apply[..$genTypes](..$termArgs) =
                ml.wolfe.term.ConstructProduct(Seq(..$caseClassArgNames),construct[..$genTypenames])

              def unapply[..$genTypes](term:ml.wolfe.term.Term[$caseClassTypeName[..$genTypenames]]) =
                Some(..$getters)
            }
            def Values[..$genTypes](..$domArgs) =
              ml.wolfe.term.ProductDom(Seq(..$caseClassArgNames), construct[..$genTypenames])
          }
            """

//        println(newTerm)
        c.Expr[Any](newTerm)
      case _ => c.abort(c.enclosingPosition, "Can't create domain")
    }
  }

}


class termdef extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro CaseClassTerm.implOnClass
}


