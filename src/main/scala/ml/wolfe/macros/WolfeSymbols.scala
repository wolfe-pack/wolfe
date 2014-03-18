package ml.wolfe.macros

import scala.reflect.api.Universe
import scala.reflect.macros.Context

/**
 * @author Sebastian Riedel
 */
trait WolfeSymbols[C <: Context] extends HasContext[C] {

  import context.universe._

  object symbols {

    lazy val wolfe     = rootMirror.staticModule("ml.wolfe.Wolfe")
    lazy val wolfeType = wolfe.typeSignature
    lazy val all       = wolfeType.member(newTermName("all"))
    lazy val unwrap2   = wolfeType.member(newTermName("unwrap2"))
    lazy val unwraps   = Range(2, 6).map(i => wolfeType.member(newTermName("unwrap" + i))).toSet
    lazy val crosses   = Range(2, 4).map(i => wolfeType.member(newTermName("Cross" + i))).toSet
    lazy val Pred      = wolfeType.member(newTermName("Pred"))
    lazy val preds     = wolfeType.member(newTermName("preds"))

    lazy val scalaSymbol     = rootMirror.staticPackage("scala")
    lazy val scalaType       = scalaSymbol.typeSignature
    lazy val TupleCompanions = Range(2, 6).map(i => scalaType.member(newTermName("Tuple" + i))).toSet
  }

  //println("Blah: " + wolfe)
}

