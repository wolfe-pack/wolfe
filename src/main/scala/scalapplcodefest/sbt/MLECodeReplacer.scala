package scalapplcodefest.sbt

import scala.tools.nsc.Global

import scalapplcodefest.Wolfe


/**
 * @author Sebastian Riedel
 */
object MLECodeReplacer extends CodeStringReplacer {
  def replace(global: Global)(tree: global.type#Tree, modification: ModifiedSourceText): Boolean = {
    import global._

    val wolfe = rootMirror.getModuleByName(newTermName("scalapplcodefest.Wolfe"))
    val argmin = definitions.getMember(wolfe, newTermName("argmin"))
    val sum = definitions.getMember(wolfe, newTermName("sum"))
    val logZ = definitions.getMember(wolfe, newTermName("logZ"))



    object ApplyCurried2 {
      def unapply(tree: Tree) = tree match {
        case Apply(Apply(TypeApply(se, types), List(arg1)), List(arg2)) => Some(se, types, arg1, arg2)
        case _ => None
      }
    }
    object ApplyCurried3 {
      def unapply(tree: Tree) = tree match {
        case Apply(Apply(Apply(TypeApply(se, types), List(arg1)), List(arg2)), List(arg3)) => Some(se, types, arg1, arg2, arg3)
        case _ => None
      }
    }

    class ApplyOperator2(val sym: Symbol) {
      def unapply(tree: Tree) = tree match {
        case ApplyCurried2(se, types, dom, obj) if se.symbol == sym => Some(types, dom, obj)
        case _ => None
      }
    }
    class ApplyOperator3(val sym: Symbol) {
      def unapply(tree: Tree) = tree match {
        case ApplyCurried3(se, types, dom, obj, num) if se.symbol == sym => Some(types, dom, obj, num)
        case _ => None
      }
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


    tree match {
      case ApplyArgmin(_, vectors, LogLikelihood(data, domain, f, w)) =>
        val featName = f.symbol.name.toString
        val sum = s"""sum ($data) { y_i => $featName(y_i) } mapValues(w => math.log(w / $data.size)) """
        modification.replace(tree.pos.start, tree.pos.end, sum)
        true
      case _ =>
        false
    }
  }

  def main(args: Array[String]) {
    GenerateSources.generate(
      sourcePath = "src/main/scala/scalapplcodefest/sbt/CoinTossingToBeCompiled.scala",
      replacers = List(MLECodeReplacer))
  }

}

