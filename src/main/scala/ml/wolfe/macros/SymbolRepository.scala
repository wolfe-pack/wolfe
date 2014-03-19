package ml.wolfe.macros

import scala.reflect.macros.Context

/**
 * @author Sebastian Riedel
 */
trait SymbolRepository[C <: Context] extends HasContext[C] {

  import context.universe._

  object wolfeSymbols {

    lazy val wolfe     = rootMirror.staticModule("ml.wolfe.Wolfe")
    lazy val wolfeType = wolfe.typeSignature
    lazy val all       = wolfeType.member(newTermName("all"))
    lazy val unwrap2   = wolfeType.member(newTermName("unwrap2"))
    lazy val unwraps   = Range(2, 6).map(i => wolfeType.member(newTermName("unwrap" + i))).toSet
    lazy val crosses   = Range(2, 4).map(i => wolfeType.member(newTermName("Cross" + i))).toSet
    lazy val Pred      = wolfeType.member(newTermName("Pred"))
    lazy val preds     = wolfeType.member(newTermName("preds"))

    lazy val atomic = rootMirror.staticClass("ml.wolfe.macros.OptimizedWolfe.Atomic")

  }

  object scalaSymbols {
    lazy val scalaSymbol     = rootMirror.staticPackage("scala")
    lazy val scalaType       = scalaSymbol.typeSignature
    lazy val TupleCompanions = Range(2, 6).map(i => scalaType.member(newTermName("Tuple" + i))).toSet
    lazy val booleanClass    = rootMirror.staticClass("scala.Boolean")
    lazy val doubleClass     = rootMirror.staticClass("scala.Double")
    lazy val and             = booleanClass.typeSignature.member(newTermName("$amp$amp"))
    lazy val doublePlus      = doubleClass.typeSignature.member(newTermName("$plus"))
    lazy val doublePluses    = doublePlus.asTerm.alternatives.toSet

  }

  //println("Blah: " + wolfe)
}

