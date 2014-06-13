package ml.wolfe.macros

import scala.reflect.macros.Context

/**
 * @author Sebastian Riedel
 */
trait SymbolRepository[C <: Context] extends HasContext[C] {

  import context.universe._

  object wolfeSymbols {

    lazy val wolfe        = rootMirror.staticModule("ml.wolfe.Wolfe")
    lazy val optimizedOps = rootMirror.staticModule("ml.wolfe.macros.OptimizedOperators")

    lazy val wolfeType        = wolfe.typeSignature
    lazy val optimizedOpsType = optimizedOps.typeSignature

    lazy val vectorClass      = rootMirror.staticClass("ml.wolfe.Wolfe.Vector")
    lazy val overWhereOfClass = rootMirror.staticClass("ml.wolfe.Wolfe.Builder")
    lazy val allClass         = rootMirror.staticClass("ml.wolfe.Wolfe.All")


    lazy val strings = wolfeType.member(newTermName("strings"))
    lazy val doubles = wolfeType.member(newTermName("doubles"))
    lazy val vectors = wolfeType.member(newTermName("vectors"))

    lazy val all                     = wolfeType.member(newTermName("all"))
    lazy val cartesianProductBuilder = wolfeType.member(newTermName("CartesianProductBuilder"))
    lazy val unwrap2                 = wolfeType.member(newTermName("unwrap2"))
    lazy val hide                    = wolfeType.member(newTermName("hide"))
    lazy val unwraps                 = Range(2, 6).map(i => wolfeType.member(newTermName("unwrap" + i))).toSet
    lazy val crosses                 = Range(2, 6).map(i => wolfeType.member(newTermName("Cross" + i))).toSet
    lazy val cs                      = wolfeType.member(newTermName("c")).asTerm.alternatives.toSet
    lazy val crossProducts           = crosses ++ cs
    lazy val Pred                    = wolfeType.member(newTermName("Pred"))
    lazy val preds                   = wolfeType.member(newTermName("preds"))
    lazy val seqs                    = wolfeType.member(newTermName("seqs")).asTerm.alternatives.toSet
    //todo: this should be based on the types, not on the names
    lazy val fixedLengthSeqs         = seqs.find(_.asMethod.paramss.head.head.name.encoded == "length").get
    lazy val maxLengthSeqs           = seqs.find(_.asMethod.paramss.head.head.name.encoded == "dom").get
    lazy val signature2seqs          = seqs.map(s => s.typeSignature -> s).toMap

    //problem builder
    lazy val over  = wolfeType.member(newTermName("over"))
    lazy val of    = overWhereOfClass.typeSignature.member(newTermName("of"))
    lazy val where = overWhereOfClass.typeSignature.member(newTermName("where"))
    lazy val using = overWhereOfClass.typeSignature.member(newTermName("using"))
    lazy val st    = overWhereOfClass.typeSignature.member(newTermName("st"))

    //optimized operators
    lazy val sums     = optimizedOpsType.member(newTermName("sum")).asTerm.alternatives.toSet
    lazy val argmaxes = optimizedOpsType.member(newTermName("argmax")).asTerm.alternatives.toSet
    lazy val maxes    = optimizedOpsType.member(newTermName("max")).asTerm.alternatives.toSet
    lazy val maps     = optimizedOpsType.member(newTermName("map")).asTerm.alternatives.toSet


    lazy val vectorType   = rootMirror.staticClass("ml.wolfe.Wolfe.Vector")
    lazy val dot          = vectorClass.typeSignature.member(newTermName("dot"))
    lazy val vectorPlus   = vectorClass.typeSignature.member(newTermName("$plus"))
    lazy val vectorPluses = vectorPlus.asTerm.alternatives.toSet
    lazy val oneHot       = rootMirror.staticClass("ml.wolfe.Wolfe.oneHot")


    lazy val atomic          = rootMirror.staticClass("ml.wolfe.Wolfe.Atomic")
    lazy val potential       = rootMirror.staticClass("ml.wolfe.Wolfe.Potential")
    lazy val optByInference  = rootMirror.staticClass("ml.wolfe.Wolfe.OptimizeByInference")
    lazy val logZByInference = rootMirror.staticClass("ml.wolfe.Wolfe.LogZByInference")
    lazy val optByLearning   = rootMirror.staticClass("ml.wolfe.Wolfe.OptimizeByLearning")
    lazy val inlinable       = rootMirror.staticClass("ml.wolfe.macros.Inlinable")


  }

  object scalaSymbols {
    lazy val booleanClass         = rootMirror.staticClass("scala.Boolean")
    lazy val doubleClass          = rootMirror.staticClass("scala.Double")
    lazy val traversableOnceClass = rootMirror.staticClass("scala.collection.TraversableOnce")
    lazy val traversableLikeClass = rootMirror.staticClass("scala.collection.TraversableLike")
    lazy val iterableClass        = rootMirror.staticClass("scala.collection.Iterable")

    lazy val scalaSymbol     = rootMirror.staticPackage("scala")
    lazy val scalaType       = scalaSymbol.typeSignature
    lazy val TupleCompanions = Range(2, 6).map(i => scalaType.member(newTermName("Tuple" + i))).toSet

    lazy val and           = booleanClass.typeSignature.member(newTermName("$amp$amp"))
    lazy val doublePlus    = doubleClass.typeSignature.member(newTermName("$plus"))
    lazy val doubleMinus   = doubleClass.typeSignature.member(newTermName("$minus"))
    lazy val doublePluses  = doublePlus.asTerm.alternatives.toSet
    lazy val doubleMinuses = doubleMinus.asTerm.alternatives.toSet

    lazy val sum    = traversableOnceClass.typeSignature.member(newTermName("sum"))
    lazy val max    = traversableOnceClass.typeSignature.member(newTermName("max"))
    lazy val maxBy  = traversableOnceClass.typeSignature.member(newTermName("maxBy"))
    lazy val map    = traversableLikeClass.typeSignature.member(newTermName("map"))
    lazy val filter = traversableLikeClass.typeSignature.member(newTermName("filter"))


  }

  //println("Blah: " + wolfe)
}

