package ml.wolfe.mv

/**
 * @author Sebastian Riedel
 */
object AAAI15 {

  //User Space
  sealed trait MyList[+T]
  case class MyCons[T](h: T, t: MyList[T]) extends MyList[T]
  case object MyNil extends MyList[Nothing]
  type MyMap[A, B] = MyList[(A, B)]
  case class World(smokes: MyMap[String, Boolean], cancer: MyMap[String, Boolean])

  val persons = List("Bob", "Anna")

  def listGet[T](index: Int, list: MyList[T]): T = list match {
    case MyNil => sys.error("Index out of bounds")
    case MyCons(h, t) => if (index == 0) h else listGet(index - 1, t)
  }

  def mapGet[A, B](key: A, map: MyMap[A, B]): B = map match {
    case MyNil => sys.error("Key not in map")
    case MyCons((k, v), t) => if (key == k) v else mapGet(key, t)
  }


  def sum[T](dom: List[T])(obj: T => Double): Double = dom match {
    case Nil => 0.0
    case h :: t => obj(h) + sum(t)(obj)
  }

  def dot[K](keys: List[K])(arg1: MyMap[K, Double], arg2: MyMap[K, Double]) = {
    sum(keys) { k => mapGet(k, arg1) * mapGet(k, arg2) }
  }

  val keys = List("w")

  def I(pred: Boolean) = if (pred) 1.0 else 0.0

  def model(y: World) =
    sum(persons) { p => 2.1 * I(!mapGet(p, y.smokes) || mapGet(p, y.cancer)) }

  def feat(y: World) =
    MyCons("w" -> sum(persons) { p => I(!mapGet(p, y.smokes) || mapGet(p, y.cancer)) }, MyNil)

  def linear(w: MyMap[String, Double])(y: World) =
    dot(keys)(w, feat(y))


  //Converted Space
  //Generic Data structures
  class State(map: Map[Any, Any]) {
    def apply[T](v: Var[T]): T = map(v).asInstanceOf[T]
  }
  class Var[T](dom: List[T]) extends Term[T] {
    def apply(state: State) = state(this)
    def vars = List(this)
  }
  case class Generic[T](f: State => T, terms: List[Term[_]]) extends Term[T] {
    def apply(state: State) = f(state)
    def vars = terms flatMap (_.vars)
  }
  trait Term[+T] {
    def apply(state: State): T
    def vars:List[Var[_]]
  }
  case class Const[+T](value: T) extends Term[T] {
    def apply(state: State) = value
    def vars = Nil
  }

  //Automatically converted structure
  sealed trait TermMyList[+T] extends Term[MyList[T]]
  case class TermMyCons[T](h: Term[T], t: TermMyList[T]) extends TermMyList[T] {
    def apply(state: State) = MyCons(h(state), t(state))
    def vars = h.vars ++ t.vars
  }
  case object TermMyNil extends TermMyList[Nothing] {
    def apply(state: State) = MyNil
    def vars = Nil
  }
  type TermMyMap[A, B] = TermMyList[(A, B)]
  case class TermWorld(smokes: TermMyMap[String, Boolean], cancer: TermMyMap[String, Boolean]) extends Term[World]{
    def apply(state: State) = World(smokes(state), cancer(state))
    def vars = smokes.vars ::: cancer.vars
  }

  def termI(pred: Term[Boolean]): Term[Double] = Generic(s => if (pred(s)) 1.0 else 0.0, List(pred))

  case class TermTuple2[T1, T2](_1: Term[T1], _2: Term[T2]) extends Term[(T1, T2)] {
    def apply(state: State) = (_1(state), _2(state))
    def vars = _1.vars ::: _2.vars
  }

  case class TermFun[A, B](f: Term[A] => Term[B],
                           dom:List[A] = sys.error("Don't know the domain of this function")) extends Term[A => B] {
    def apply(state: State) = a => f(Const(a))(state)
    def vars = dom flatMap (a => f(Const(a)).vars)
  }

  sealed trait TermList[T] extends Term[List[T]] {
  }
  case class TermCons[T](h: Term[T], t: TermList[T]) extends TermList[T] {
    def apply(s: State) = h(s) :: t(s)
    def vars = h.vars ::: t.vars
  }
  case object TermNil extends TermList[Nothing] {
    def apply(s: State) = Nil
    def vars = Nil
  }

  /*
   def mapGet[A,B](key:A, map:MyMap[A,B]):B = map match {
    case MyNil => sys.error("Key not in map")
    case Cons((k,v),t) => if (key == k) v else mapGet(key,t)
  }
   */
  def termMapGet[A, B](key: Term[A], map: TermMyMap[A, B]): Term[B] = map match {
    case TermMyNil => sys.error("Key not in map")
    case TermMyCons(TermTuple2(k: Const[_], v), t) if key.isInstanceOf[Const[_]] => if (k == key) v else termMapGet(key, t)
    case _ => ???
    //    case TermCons(TermTuple2(k:Const,v),t) => Generic(s => if (k(s) == key(s)) v(s) else termMapGet(key,t)(s))
  }

  /*
    def sum[T](dom:List[T])(obj:T => Double):Double = dom match {
    case Nil => 0.0
    case h :: t => obj(h) + sum(t)(obj)
  }
   */

  case class Plus(arg1: Term[Double], arg2: Term[Double]) extends Term[Double] {
    def apply(state: State) = arg1(state) + arg2(state)
    def vars = arg1.vars ::: arg2.vars
  }

  case class Times(arg1: Term[Double], arg2: Term[Double]) extends Term[Double] {
    def apply(state: State) = arg1(state) * arg2(state)
    def vars = arg1.vars ::: arg2.vars
  }

  def termSum[T](dom: TermList[T])(obj: TermFun[T, Double]): Term[Double] = {
    dom match {
      case TermNil => Const(0.0)
      case TermCons(h, t) => Plus(obj.f(h), termSum(t)(obj))
    }
  }

  /*
   def model(y:World) =
    sum(persons) { p => 2.1 * I(!mapGet(p,y.smokes) || mapGet(p,y.cancer))}
   */

  def toTermList[T](list: List[T]): TermList[T] = list match {
    case Nil => TermNil.asInstanceOf[TermList[T]]
    case h :: t => TermCons(Const(h), toTermList(t))
  }

  def termModel(y: TermWorld): Term[Double] = {
    termSum(toTermList(persons)) {
      TermFun(p => Times(Const(2.1), termI(
        Generic(s => !termMapGet(p, y.smokes)(s) || termMapGet(p, y.cancer)(s), List(termMapGet(p, y.smokes), termMapGet(p, y.cancer))))),
      persons)
    }
  }

  /*
   p => 2.1 * I(!mapGet(p,y.smokes) || mapGet(p,y.cancer))
   */

  //todo: do a tail recursive version and make sure it works
  def allMaps[A, B](dom: List[A], range: List[B]): List[MyMap[A, B]] = dom match {
    case Nil => List(MyNil)
    case h :: t => for (m <- allMaps(t, range); v <- range) yield MyCons((h, v), m)
  }

  def bools = List(false, true)

  def termBools = new Var(bools)
  def termPersons = new Var(persons)

  def termToList[T](term: Term[T]): List[T] = ???

  def termAllMaps[A, B](dom: Term[A], range: Term[B]): TermMyMap[A, B] = termToList(dom) match {
    case Nil => TermMyNil
    case h :: t => TermMyCons(TermTuple2(Const(h), range), termAllMaps(new Var(t), range))
  }

  def space = for (s <- allMaps(persons, bools); c <- allMaps(persons, bools)) yield World(s, c)
  def space2 = (for (s <- allMaps(persons, bools)) yield for (c <- allMaps(persons, bools)) yield World(s, c)).flatMap(identity)
  //allMaps(..) flatMap (s => allMaps(...) map (c => World(s,c))
  //allMaps(..) map (s => allMaps(...) map (c => World(s,c)) flatten
  //List(1,2,3).permutations

  val termSpace = TermWorld(termAllMaps(termPersons, termBools), termAllMaps(termPersons, termBools))

  val fg = termModel(termSpace)

  //unroll(fg)
  //state = infer(unroll(fg))
  //termSpace(state)


  /*
    def feat(y:World):MyMap[String,Double] =
      MyCons("w" -> sum(persons) { p => I(!mapGet(p,y.smokes) || mapGet(p,y.cancer)) }, MyNil)

    def linear(w:MyMap[String,Double])(y:World) =
      dot(keys)(w,feat(y))

    def dot[K](keys: List[K])(arg1: MyMap[K,Double], arg2: MyMap[K,Double]) = {
      sum(keys) { k => mapGet(k,arg1) * mapGet(k,arg2) }
  }

   */

  def toConst[T](t:Term[T]):Const[T] = t match {
    case c:Const[_] => c.asInstanceOf[Const[T]]
    case _ => sys.error("Damn")
  }

  def termDot[K](keys: TermList[K])(arg1: TermMyMap[K, Double], arg2: TermMyMap[K, Double]): Term[Double] = {
    termSum(keys) { TermFun(k => Times(termMapGet(k, arg1), termMapGet(k, arg2)), toConst(keys).value) }
  }

  def termFeat(y: TermWorld): TermMyMap[String, Double] = {
    TermMyCons(TermTuple2(Const("w"), termSum(toTermList(persons)) {
      TermFun(p => termI(
        Generic(s => !termMapGet(p, y.smokes)(s) || termMapGet(p, y.cancer)(s), List(termMapGet(p, y.smokes), termMapGet(p, y.cancer)))
      ), persons)
    }), TermMyNil)
  }

  def termLinear(w: TermMyMap[String, Double])(y: TermWorld): Term[Double] = {
    termDot(toTermList(keys))(w, termFeat(y))
  }



}
