package scalapplcodefest.sbt

import scalapplcodefest.Wolfe.Stats.OneHot


/**
 * @author Sebastian Riedel
 */
trait WolfePatterns {

  this: InGeneratorEnvironment =>

  import env.global._

  val wolfe = rootMirror.getModuleByName(newTermName("scalapplcodefest.Wolfe"))
  val argmin = definitions.getMember(wolfe, newTermName("argmin"))
  val sum = definitions.getMember(wolfe, newTermName("sum"))
  val logZ = definitions.getMember(wolfe, newTermName("logZ"))
  val stats = definitions.getMemberModule(wolfe, newTermName("Stats"))

  //search space generators
  val all = definitions.getMember(wolfe, newTermName("all"))
  val cross2 = definitions.getMember(wolfe, newTermName("c"))
  val unwrap2 = definitions.getMember(wolfe, newTermName("unwrap2"))
  val bools = definitions.getMember(wolfe, newTermName("bools"))
  val doubles = definitions.getMember(wolfe, newTermName("doubles"))


  //new core operators
  val sum2 = definitions.getMember(wolfe, newTermName("sum2"))
  val argmax2 = definitions.getMember(wolfe, newTermName("argmax2"))

  //new derived operators
  val logZ2 = definitions.getMember(wolfe, newTermName("logZ2"))
  val argmin2 = definitions.getMember(wolfe, newTermName("argmin2"))

  //sufficient stats
  val oneHot = definitions.getMember(wolfe, newTermName("oneHot"))

  //Annotations
  val MarkerOneHot = definitions.getMemberClass(stats, newTermName("OneHot"))



  object ApplyCurried2 {
    def unapply(tree: Tree) = tree match {
      case Apply(Apply(TypeApply(se, types), List(arg1)), List(arg2)) =>
        Some(se, types, arg1, arg2)
      case _ => None
    }
  }


  object ApplyCurried3 {
    def unapply(tree: Tree) = tree match {
      case Apply(Apply(Apply(TypeApply(se, types), List(arg1)), List(arg2)), List(arg3)) =>
        Some(se, types, arg1, arg2, arg3)
      case _ => None
    }
  }

  object ApplyCurried4 {
    def unapply(tree: Tree) = tree match {
      case Apply(Apply(Apply(Apply(TypeApply(se, types), List(arg1)), List(arg2)), List(arg3)), List(arg4)) =>
        Some(se, types, arg1, arg2, arg3, arg4)
      case _ => None
    }

    def apply(select: Tree, types: List[Tree], arg1: Tree, arg2: Tree, arg3: Tree, arg4: Tree) =
      Apply(Apply(Apply(Apply(TypeApply(select, types), List(arg1)), List(arg2)), List(arg3)), List(arg4))
  }


  class ApplyOperator2(val sym: Symbol) {
    def unapply(tree: Tree) = tree match {
      case ApplyCurried2(se, types, dom, obj) if se.symbol == sym => Some(types, dom, obj)
      case _ => None
    }
    def apply(types: List[Tree], dom: Tree, obj: Tree) = null

  }

  class ApplyOperator3(val sym: Symbol) {
    def unapply(tree: Tree) = tree match {
      case ApplyCurried3(se, types, dom, obj, num) if se.symbol == sym => Some(types, dom, obj, num)
      case _ => None
    }
    def apply(types: List[Tree], dom: Tree, obj: Tree, num: Tree) = null

  }

  class ApplyOperator4(val sym: Symbol) {
    def unapply(tree: Tree) = tree match {
      case ApplyCurried4(se, types, dom, pred, obj, meta) if se.symbol == sym => Some(types, dom, pred, obj, meta)
      case _ => None
    }
    def apply(types: List[Tree], dom: Tree, pred: Tree, obj: Tree, num: Tree) =
      ApplyCurried4(Select(???, sym), types, dom, pred, obj, num)

  }


  object DotProduct {
    def unapply(tree: Tree) = tree match {
      case Apply(Select(Apply(_, List(arg1)), _), arg2) => Some(arg1, arg2)
      case _ => None
    }
  }

  object PerInstanceLogLikelihood {
    def unapply(tree: Tree) = tree match {
      case Function(y_i, Apply(Select(ApplyLogZ(_, domain, Function(y, LinearModel(f1, _, w1))), minus), List(LinearModel(f2, _, w2))))
        if f1.symbol.name == f2.symbol.name =>
        //todo: should check model
        Some(domain, f1, w1)
      case _ => None
    }
  }

  object LogLikelihood {
    def unapply(tree: Tree) = tree match {
      case Function(weight1, ApplySum(_, data, PerInstanceLogLikelihood(domain, f, w), _)) => Some(data, domain, f, w)
      case _ => None
    }
  }


  object LinearModel {
    def unapply(tree: Tree) = tree match {
      case DotProduct(Apply(f, y), w) => Some(f, y, w)
      case _ => None
    }
  }

  object ApplyArgmin extends ApplyOperator2(argmin)
  object ApplySum extends ApplyOperator3(sum)
  object ApplyLogZ extends ApplyOperator2(logZ)




  object ApplyArgmin2 extends ApplyOperator4(argmin2)
  object ApplyArgmax2 extends ApplyOperator4(argmax2)
  object ApplyLogZ2 extends ApplyOperator3(logZ2)

  object PerInstanceLogLikelihood2 {
    def unapply(tree: Tree) = tree match {
      case Function(y_i, Apply(Select(ApplyLogZ2(_, domain, condition, Function(y, LinearModel(f1, _, w1))), minus), List(LinearModel(f2, _, w2))))
        if f1.symbol.name == f2.symbol.name =>
        //todo: should check model
        Some(domain, f1, w1)
      case _ => None
    }
  }

  object LogLikelihood2 {
    def unapply(tree: Tree) = tree match {
      case Function(weight1, ApplySum(_, data, PerInstanceLogLikelihood2(domain, f, w), _)) => Some(data, domain, f, w)
      case _ => None
    }
  }



}
