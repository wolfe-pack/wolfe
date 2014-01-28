package scalapplcodefest.sbt


/**
 * @author Sebastian Riedel
 */
trait WolfePatterns {

  this: InGeneratorEnvironment =>

  import env.global._

  val scalapplcodefest = rootMirror.getPackage(newTermName("scalapplcodefest"))
  val wolfe            = rootMirror.getModuleByName(newTermName("scalapplcodefest.Wolfe"))

  val stats     = definitions.getMemberModule(wolfe, newTermName("Stats"))
  val objective = definitions.getMemberModule(wolfe, newTermName("Objective"))


  //search space generators
  val all     = definitions.getMember(wolfe, newTermName("all"))
  val cross2  = definitions.getMember(wolfe, newTermName("c"))
  val unwrap2 = definitions.getMember(wolfe, newTermName("unwrap2"))
  val bools   = definitions.getMember(wolfe, newTermName("bools"))
  val doubles = definitions.getMember(wolfe, newTermName("doubles"))


  //new core operators
  val sum    = definitions.getMember(wolfe, newTermName("sum"))
  val argmax = definitions.getMember(wolfe, newTermName("argmax"))

  //new derived operators
  val logZ   = definitions.getMember(wolfe, newTermName("logZ"))
  val argmin = definitions.getMember(wolfe, newTermName("argmin"))
  val max    = definitions.getMember(wolfe, newTermName("max"))


  //sufficient stats
  val oneHot = definitions.getMember(wolfe, newTermName("oneHot"))

  //Annotations
  val MarkerOneHot         = definitions.getMemberClass(stats, newTermName("OneHot"))
  val MarkerDifferentiable = definitions.getMemberClass(objective, newTermName("Differentiable"))

  trait ApplyBinaryOperator {
    def unapply(tree: Tree): Option[(Tree, Tree)]
  }

  class ApplyDoubleOperator(name: String) extends ApplyBinaryOperator {
    def unapply(tree: Tree) = tree match {
      case Apply(s@Select(arg1, opName), List(arg2))
        if s.symbol.owner == definitions.DoubleClass && opName.encoded == name => Some(arg1, arg2)
      case _ => None
    }
  }

  object ApplyDoubleMinus extends ApplyDoubleOperator("$minus")
  object ApplyDoublePlus extends ApplyDoubleOperator("$plus")
  object ApplyDoubleTimes extends ApplyDoubleOperator("$times")

  class Flattened(operator: ApplyBinaryOperator) {
    val Match = this
    def unapply(tree: Tree): Option[List[Tree]] = tree match {
      case operator(Match(args1), Match(args2)) => Some(args1 ::: args2)
      case operator(arg1, Match(args2)) => Some(arg1 :: args2)
      case operator(Match(args1), arg2) => Some(arg2 :: args1)
      case operator(arg1, arg2) => Some(List(arg1, arg2))
      case _ => None
    }
  }

  object FlatDoubleSum extends Flattened(ApplyDoublePlus)

  object FlatDoubleProduct {
    def unapply(tree: Tree): Option[List[Tree]] = tree match {
      case ApplyDoubleTimes(FlatDoubleProduct(args1), FlatDoubleProduct(args2)) => Some(args1 ::: args2)
      case ApplyDoubleTimes(arg1, FlatDoubleProduct(args2)) => Some(arg1 :: args2)
      case ApplyDoubleTimes(FlatDoubleProduct(args1), arg2) => Some(arg2 :: args1)
      case ApplyDoubleTimes(arg1, arg2) => Some(List(arg1, arg2))
      case _ => None
    }
  }


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
      case ApplyCurried4(se, types, dom, pred, obj, meta) if se.symbol == sym => Some(se, types, dom, pred, obj, meta)
      case _ => None
    }
    def apply(select: Tree, types: List[Tree], dom: Tree, pred: Tree, obj: Tree, num: Tree) =
      ApplyCurried4(select, types, dom, pred, obj, num)
  }


  object DotProduct {
    def unapply(tree: Tree) = tree match {
      case Apply(Select(Apply(_, List(arg1)), _), List(arg2)) => Some(arg1, arg2)
      case _ => None
    }
  }


  object LinearModel {
    def unapply(tree: Tree) = tree match {
      case DotProduct(Apply(f, y), w) => Some(f, y, w)
      case _ => None
    }
  }

  object EqualityConditions {
    def unapply(tree: Tree) = tree match {
      case Function(List(dataArgDef), FieldEquality(dataArgUse, fieldName, conditionValue)) =>
        Some(List(fieldName -> conditionValue))
      case _ => None
    }
  }


  object ApplyArgmin extends ApplyOperator4(argmin)
  object ApplyArgmax extends ApplyOperator4(argmax)
  object ApplyMax extends ApplyOperator4(max)
  object ApplyLogZ extends ApplyOperator3(logZ)
  object ApplySum extends ApplyOperator4(sum)


  object PerInstanceLogLikelihood {
    def unapply(tree: Tree) = tree match {
      case Function(y_i, Apply(Select(ApplyLogZ(_, domain, condition, Function(y, LinearModel(f1, _, w1))), minus), List(LinearModel(f2, _, w2))))
        if f1.symbol.name == f2.symbol.name =>
        //todo: should check model
        Some(domain, f1, w1)
      case _ => None
    }
  }

  object LogLikelihood {
    def unapply(tree: Tree) = tree match {
      case Function(weight1, ApplySum(_, _, data, _, PerInstanceLogLikelihood(domain, f, w), _)) => Some(data, domain, f, w)
      case _ => None
    }
  }


  /**
   * Extracts whether a set expression corresponds to a set of case class objects, and
   * returns the fields of the case class and the sets that make up the arguments
   * of the cartesian product that forms the set of case class objects.
   */
  object CaseClassDomain {
    def unapply(tree: Tree) = tree match {
      case Apply(Apply(TypeApply(allTuples, _), List(Apply(unwrap, List(constructor)))), List(Apply(TypeApply(cross, _), sets))) =>
        env.moduleDefs.get(constructor.symbol) match {
          case Some(cdef) =>
            val fields = cdef.impl.body.collectFirst({
              case DefDef(_, name, _, List(params), _, _) if name.encoded == "apply" => params
            })
            Some(constructor, fields.get, sets)
          case _ => None
        }
      case _ => None
    }
    def apply(constructor: Tree, fields: List[ValDef], sets: List[Tree]) = ???
  }

  /**
   * Detects tests for field equality with a specific value.
   */
  object FieldEquality {
    def unapply(tree: Tree) = tree match {
      case Apply(Select(Select(data, fieldName), eqeq), List(rhs)) =>
        //todo: check whether eqeq is an equals method
        Some(data, fieldName, rhs)
      case Select(data, fieldName) =>
        //we assume that this select is used in a context where a boolean is expected
        Some(data, fieldName, Literal(Constant(true)))
      case _ => None
    }
    def apply(dataObject: Tree, fieldName: Name, rhs: Tree) = ???
  }


}
