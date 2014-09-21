package ml.wolfe.mv

import ml.wolfe.macros.{ContextHelper, CodeRepository}

import scala.reflect.macros.Context
import scala.language.experimental.macros

case class State(map: Map[Leaf[_], Any]) {
  def get[T](leaf: Leaf[T]): Option[T] = map.get(leaf).asInstanceOf[Option[T]]
  def apply[T](leaf: Leaf[T]) = get(leaf).get
}
/**
 * @author Sebastian Riedel
 */
sealed trait Element[+T] {
  def eval(state: State): T
}
case class Const[+T](value: T) extends SearchSpaceTree[T] {
  def eval(state: State) = value
}
case class Plus(arg1: Element[Double], arg2: Element[Double]) extends Element[Double] {
  def eval(state: State) = arg1.eval(state) + arg2.eval(state)
}
case class GenFunApp[A, +B](f: Element[A => B], a: Element[A]) extends Element[B] {
  def eval(state: State) = f.eval(state)(a.eval(state))
}
//case class Generic[A,B](f:A=>B,a:Element[A]) extends Element[B]
//should be GenFunApp(Const(f),a)

sealed trait SearchSpaceTree[+T] extends Element[T]
case class Leaf[+T](id: Any, dom: List[T]) extends SearchSpaceTree[T] {
  def eval(state: State) = state(this)
}
case class Nonterminal[+T1, +T2, S1 <: SearchSpaceTree[T1], S2 <: SearchSpaceTree[T2]](fst: S1, snd: S2) extends SearchSpaceTree[(T1, T2)] {
  def eval(state: State) = (fst.eval(state), snd.eval(state))
}

case class ADTNonterminal[T1, T2, S1 <: SearchSpaceTree[T1], S2 <: SearchSpaceTree[T2], ADT](fst: S1, snd: S2, adt: (T1, T2) => ADT) extends SearchSpaceTree[ADT] {
  def eval(state: State) = adt(fst.eval(state), snd.eval(state))
}

object SearchSpace {
  def unit[T](elem: T) = List(elem)
  def cross[T1, T2](dom1: List[T1], dom2: List[T2]) = for (a1 <- dom1; a2 <- dom2) yield (a1, a2)
  def crossADT[T1, T2, A](adt: (T1, T2) => A)(dom1: List[T1], dom2: List[T2]) = for (a1 <- dom1; a2 <- dom2) yield adt(a1, a2)

}

object Transformer {

  trait ModelTransformer[C <: Context] extends CodeRepository[C] {

    import context.universe._

    def transformedTypePrediction(tree: Tree): Map[Symbol, Type] = {
      val collected = tree.collect({
        case Apply(i@Ident(sym), args) =>
          //transform arguments
          val tArgs = args map transformSpace
          i.symbol -> MethodType(tArgs map (_.symbol), null)
      })
      collected.toMap withDefault (sym => {
        ???
      })
    }

    def transformSpace(space: Tree): Tree = {
      val transformedTypes = transformedTypePrediction(space)
      val transformed = transform(space, {
        case id@Ident(name) =>
          val typ = transformedTypes(id.symbol)
          get(id.symbol) match {
            case Some(defdef) =>
            case _ =>
          }
          id
      })
      //first do minimal transformed type inference for all identifiers in the expression
      //if object is identifier, figure out its minimal transformed type in the context,
      //then translate
      //first replace tree w/o creating function definitions
      //then figure out converted argument types of converted functions
      //then convert
      q"null"
    }

    def transformModel(space: Tree, obj: Tree): Tree = {
      val spaceTreeExpr = transformSpace(space)
      q"($spaceTreeExpr,null)"
    }

  }

  def transformModel[T](space: List[T])(obj: T => Double): (SearchSpaceTree[T], Element[Double]) = macro transformModelImpl[T]

  def transformModelImpl[T](c: Context)
                           (space: c.Expr[List[T]])
                           (obj: c.Expr[T => Double]): c.Expr[(SearchSpaceTree[T], Element[Double])] = {
    val helper = new ContextHelper[c.type](c) with ModelTransformer[c.type]
    val transformed = helper.transformModel(space.tree, obj.tree)
    c.Expr[(SearchSpaceTree[T], Element[Double])](transformed)
  }

}










