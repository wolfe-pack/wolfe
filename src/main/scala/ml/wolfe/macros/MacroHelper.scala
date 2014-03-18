package ml.wolfe.macros

import scala.reflect.macros.Context

/**
 * Class that takes a context and provides all kinds of support methods
 * for providing optimized macro expansions.
 *
 * @author Sebastian Riedel
 */
class MacroHelper[C <: Context](val context: C) extends TransformHelper[C]
                                                        with StructuredGraphHelper[C]
                                                        with GradientBasedMinimizationHelper[C]
                                                        with HasContext[C] {

  import context.universe._

  //mapping from val names to definitions (we use methods with no arguments as vals too)
  val vals = context.enclosingUnit.body.collect({
    case ValDef(_, name, _, rhs) => name -> rhs
    case DefDef(_, name, _, Nil, _, rhs) => name -> rhs
  }).toMap

  //mapping from val names to definitions (we use methods with no arguments as vals too)
  val valDefs = context.enclosingUnit.body.collect({
    case v: ValDef if !v.rhs.isEmpty => v.symbol -> v.rhs
    case d: DefDef if d.vparamss == Nil => d.symbol -> d.rhs
  }).toMap

  //mapping from def $name = rhs definitions to rhs
  val noArgDefDefs = context.enclosingUnit.body.collect({
    case d: DefDef if d.vparamss == Nil => d.symbol -> d.rhs
  }).toMap

  //mapping from symbols to methods
  val defs = context.enclosingUnit.body.collect({
    case d: DefDef if d.vparamss != Nil => d.symbol -> d
  }).toMap

  //todo: should this be looking up by symbol?
  val classes = context.enclosingUnit.body.collect({
    case cd@ClassDef(_, name, _, _) => name -> cd
  }).toMap
  //todo: should this be looking up by symbol?
  //  val classes = context.enclosingRun.units.flatMap(_.body.collect({
  //    case cd@ClassDef(_, name, _, _) => name -> cd
  //  })).toMap

  def getAnnotationArgs(tree: Tree, name: String): Option[List[Tree]] = {
    tree.symbol.annotations.collectFirst({
      //todo: use tpe symbol and check against symbol
      case Annotation(tpe, args, _) if tpe.typeSymbol.name.encoded == name => args
    })
  }

  def getMaxByProcedure(tree: Tree) = simplifyBlocks(tree) match {
    case q"(${_}) => $f1(${_})(${_})" => getAnnotationArgs(f1, "MaxByInference") match {
      case Some(List(arg)) => arg
      case _ => q"MaxProduct(_:MPGraph,1)"
    }
    case _ => q"MaxProduct(_:MPGraph,1)"
  }


}


