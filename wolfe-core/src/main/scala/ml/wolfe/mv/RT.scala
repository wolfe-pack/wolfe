package ml.wolfe.mv

import ml.wolfe.macros.{ContextHelper, CodeRepository}

import scala.reflect.macros.Context
import scala.language.experimental.macros

case class State(map: Map[RTVar[_], Any]) {
  def get[T](leaf: RTVar[T]): Option[T] = map.get(leaf).asInstanceOf[Option[T]]
  def apply[T](leaf: RTVar[T]) = get(leaf).get
}
/**
 * @author Sebastian Riedel
 */
sealed trait RT[+T] {
  def value: T
}
case class RTConst[+T](value: T) extends SearchSpaceTerm[T] {

}

case class RTGeneric[T](gen:() => T,children:List[RT[_]]) extends RT[T] {
  def value = gen()
}

case class Plus(arg1: RT[Double], arg2: RT[Double]) extends RT[Double] {
  def value = arg1.value + arg2.value
}
case class GenFunApp[A, +B](f: RT[A => B], a: RT[A]) extends RT[B] {
  def value = f.value(a.value)
}
//case class Generic[A,B](f:A=>B,a:Element[A]) extends Element[B]
//should be GenFunApp(Const(f),a)

sealed trait SearchSpaceTerm[+T] extends RT[T]
case class RTVar[T](id: Any, dom: List[T]) extends SearchSpaceTerm[T] {
  var _value:T = _
  def value = _value
}

case class RTCase2[T1, T2, S1 <: SearchSpaceTerm[T1], S2 <: SearchSpaceTerm[T2], ADT](adt: (T1, T2) => ADT,fst: S1, snd: S2) extends SearchSpaceTerm[ADT] {
  def value = adt(fst.value, snd.value)
}

object SearchSpace {
  def unit[T](elem: T) = List(elem)
  def cross[T1, T2](dom1: List[T1], dom2: List[T2]) = for (a1 <- dom1; a2 <- dom2) yield (a1, a2)
  def crossADT[T1, T2, A](adt: (T1, T2) => A)(dom1: List[T1], dom2: List[T2]) = for (a1 <- dom1; a2 <- dom2) yield adt(a1, a2)

}

object Transformer {

  trait ModelTransformer[C <: Context] extends CodeRepository[C] {

    import context.universe._

//    def transformSpace(space: Tree, arguments:Map[Symbol,Symbol]): Tree = {
//      val transformed = transform(space, {
//        case id@Ident(name) if arguments.contains(id.symbol) =>
//          Ident(arguments(id.symbol))
//        case Apply(fun,args) =>
//          ???
//        case Function(vparams,body) =>
//
//          ???
//
//      })
//      //first do minimal transformed type inference for all identifiers in the expression
//      //if object is identifier, figure out its minimal transformed type in the context,
//      //then translate
//      //first replace tree w/o creating function definitions
//      //then figure out converted argument types of converted functions
//      //then convert
//      q"null"
//    }

    sealed trait TargetType

    case class RawType(typ:Type) extends TargetType
    case class RTCaseType(caseType:Type) extends TargetType
    case class FunType(argTypes:List[TargetType],resultType:TargetType) extends TargetType

    def transformSpace(space: Tree, arguments:Map[Symbol,Symbol], target:TargetType): Tree = {
//      space match {
//        case id@Ident(name) if arguments.contains(id.symbol) =>
//          Ident(arguments(id.symbol))
//        case Apply(fun,args) =>
//          ???
//        case Function(vparams,body) =>
//          ???
//        case Select(qualifier,name) =>
//          ???
//        case TypeApply(fun,args) =>
//          ???
//
//      }
      q"null"
      //first do minimal transformed type inference for all identifiers in the expression
      //if object is identifier, figure out its minimal transformed type in the context,
      //then translate
      //first replace tree w/o creating function definitions
      //then figure out converted argument types of converted functions
      //then convert
    }


    def transformModel(space: Tree, obj: Tree): Tree = {
      val spaceTreeExpr = transformSpace(space, Map.empty, null)
      q"($spaceTreeExpr,null)"
    }

  }

  def transformModel[T](space: List[T])(obj: T => Double): (SearchSpaceTerm[T], RT[Double]) = macro transformModelImpl[T]

  def transformModelImpl[T](c: Context)
                           (space: c.Expr[List[T]])
                           (obj: c.Expr[T => Double]): c.Expr[(SearchSpaceTerm[T], RT[Double])] = {
    val helper = new ContextHelper[c.type](c) with ModelTransformer[c.type]
    val transformed = helper.transformModel(space.tree, obj.tree)
    c.Expr[(SearchSpaceTerm[T], RT[Double])](transformed)
  }

}










