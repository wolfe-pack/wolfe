package scalapplcodefest.legacy.compiler

import scala.tools.nsc.Global
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.tools.nsc.ast.TreeDSL


/**
 * User: rockt
 * Date: 1/2/14
 * Time: 2:17 PM
 */

object WolfePlayground extends App {
  val code = """
               | class A extends (() => Any) {
               |  val x = 1
               |
               |  def apply() = { println(x)}
               |  }
             """.stripMargin

  val compiler = new StringCompiler(Some(new PlaygroundTransformer), runsAfter = List("parser"))

  val virtualDir = compiler.outputDir


  compiler.compileCode(code)

  val classLoader = new AbstractFileClassLoader(virtualDir, this.getClass.getClassLoader)
  val cls = classLoader.loadClass("A") // where className is the name of the class/object in the code

  cls.getConstructor().newInstance().asInstanceOf[() => Any].apply()


  class PlaygroundTransformer extends WolfeTransformer {
    def transformTree[T <: Global#Tree](g: Global, env: WolfeCompilerPlugin2#Environment, tree: T): T = {
      import g._
      import definitions._

      val dsl = new TreeDSL {
        val global = g
      }

      import dsl.CODE._

      tree match {
        case v@ValDef(mods, name, tpt, r@Literal(value)) => {
          println("Before: " + v)

          val newType = StringClass.tpe

          val newRHS = treeCopy.Literal(r, Constant("Hello world. Old value = " + value.intValue))
          newRHS.setType(newType)

          val newName = newTermName("newX ")
          val newTPT = treeCopy.Ident(tpt, newTypeName("String"))
          newTPT.setType(newType)

          val oldSymbol = v.symbol
          val oldOwner = oldSymbol.owner

          val newSymbol = oldSymbol.cloneSymbol(oldOwner)
          newSymbol.name = newName
          newSymbol.setTypeSignature(newType)
          oldOwner.info.decls enter newSymbol

          val newVal = treeCopy.ValDef(v, mods, newName, newTPT, newRHS)
          newVal.setSymbol(newSymbol)

          println("After: " + newVal)

          newVal.asInstanceOf[T]

        }
        case d@DefDef(mods, name, tparams, vparamss, tpt, rhs@Select(qualifier, _)) => {
          println("Before: " + d)
          val mySymbol = d.symbol
          val owner = mySymbol.owner


          val newType = StringClass.tpe

          val newTPT = treeCopy.TypeTree(tpt).defineType(newType)
          newTPT.setType(newType)

          val newRHS = treeCopy.Select(rhs, qualifier, newTermName("newX "))
          newRHS.setType(newType)
          newRHS.symbol = owner.tpe.member(newTermName("newX "))

          val newDef = treeCopy.DefDef(d, mods, name, tparams, vparamss, newTPT, newRHS)

          val newSymbol = mySymbol.cloneSymbol(owner)
          newSymbol.setTypeSignature(newType)
          owner.info.decls.unlink(mySymbol)
          owner.info.decls.unlink(newSymbol)
          newDef.setSymbol(newSymbol)

          println("After: " + newDef)

          newDef.asInstanceOf[T]
        }
        case t => t
      }
    }
  }
}
