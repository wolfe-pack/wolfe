package ml.wolfe.mv

/**
 * @author Sebastian Riedel
 */
object AAAI15 extends App {

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
  class State(map: Map[Var[_], Any]) {
    def apply[T](v: Var[T]): T = map(v).asInstanceOf[T]
  }
  class Var[T](val dom: List[T]) extends Term[T] {
    def apply(state: State) = state(this)
    def vars = List(this)
    override def toString = s"Var(${dom mkString ", "})"
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
  case class TermMyCons[T](h: Term[T], t: Term[MyList[T]]) extends TermMyList[T] {
    def apply(state: State) = MyCons(h(state), t(state))
    def vars = h.vars ++ t.vars
  }
  case object TermMyNil extends TermMyList[Nothing] {
    def apply(state: State) = MyNil
    def vars = Nil
  }
  type TermMyMap[A, B] = TermMyList[(A, B)]
  case class TermWorld(smokes: Term[MyMap[String, Boolean]], cancer: Term[MyMap[String, Boolean]]) extends Term[World]{
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
  case class TermCons[T](h: Term[T], t: Term[List[T]]) extends TermList[T] {
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
  def termMapGet[A, B](key: Term[A], map: Term[MyMap[A, B]]): Term[B] = map match {
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

  def termSum[T](dom: Term[List[T]])(obj: TermFun[T, Double]): Term[Double] = {
    dom match {
      case TermNil => Const(0.0)
      case TermCons(h, t) => Plus(obj.f(h), termSum(t)(obj))
    }
  }

  /*
   def model(y:World) =
    sum(persons) { p => 2.1 * I(!mapGet(p,y.smokes) || mapGet(p,y.cancer))}

   def termModel(y:TermWorld) = Generic(s =>
    sum(persons) { p => 2.1 * I(!mapGet(p,y(s).smokes) || mapGet(p,y(s).cancer))}

   def termModel(y:TermWorld) =
    termSum(toTermList(persons)) { Generic(s => p => 2.1 * I(!mapGet(p,y(s).smokes) || mapGet(p,y(s).cancer))}

   def termModel(y:TermWorld) =
    termSum(toTermList(persons)) { TermFun(p => Generic(s => 2.1 * I(!mapGet(p,y(s).smokes) || mapGet(p,y(s).cancer))}

   def termModel(y:TermWorld) =
    termSum(toTermList(persons)) { TermFun(p => Times(Const(2.1), Generic(s => I(!mapGet(p,y(s).smokes) || mapGet(p,y(s).cancer))}

   def termModel(y:TermWorld) =
    termSum(toTermList(persons)) { TermFun(p => Times(Const(2.1), termI(Generic(s => !mapGet(p,y(s).smokes) || mapGet(p,y(s).cancer))}

   def termModel(y:TermWorld) =
    termSum(toTermList(persons)) { TermFun(p => Times(Const(2.1), termI(Generic(s => !mapGet(p,y.smokes(s)) || mapGet(p,y.cancer(s)))}

   def termModel(y:TermWorld) =
    termSum(toTermList(persons)) { TermFun(p => Times(Const(2.1), termI(Generic(s => !termMapGet(p,y.smokes)(s) || termMapGet(p,y.cancer)(s))}

    ---------------

   def termModel(y:TermWorld)(implicit s: State): Double =
    sum(persons) { p => 2.1 * I(!mapGet(p,y(s).smokes) || mapGet(p,y(s).cancer))}


   def termModel(y:TermWorld): Double =
    sum(persons) { p => 2.1 * I(!mapGet(p,y(s).smokes) || mapGet(p,y(s).cancer))}

   def termModel(y:TermWorld): TermDouble =
    sum(persons) { p => 2.1 * I(!mapGet(p,y.smokes(s)) || mapGet(p,y.cancer(s)))}

   def termModel(y:TermWorld): TermDouble =
    sum(persons) { p => 2.1 * I(!termMapGet(p,y.smokes)(s) || termMapGet(p,y.cancer)(s))}

   def termModel(y:TermWorld): TermDouble =
    sum(persons) { p => 2.1 * I(Generic(sq => !termMapGet(p,y.smokes)(sq) || termMapGet(p,y.cancer)(s))}

   def termModel(y:TermWorld): TermDouble =
    sum(persons) { p => 2.1 * I(Generic(sq => !termMapGet(p,y.smokes)(sq) || termMapGet(p,y.cancer)(sq)(s))}

   def termModel(y:TermWorld): TermDouble =
    sum(persons) { p => 2.1 * termI(Generic(sp => !Generic(sq => termMapGet(p,y.smokes)(sq))(sp) || termMapGet(p,y.cancer)(sp)), List())(s)}

   def termModel(y:TermWorld): TermDouble =
    sum(persons) { p => Times(Const(2.1), termI(Generic(sp => !termMapGet(p,y.smokes)(sp) || termMapGet(p,y.cancer)(sp)), List())(s)}

   def termModel(y:TermWorld): TermDouble =
    sum(persons) { TermFun(p => Times(Const(2.1), termI(Generic(sp => !termMapGet(p,y.smokes)(sp) || termMapGet(p,y.cancer)(sp)), List()))(s)}

   def termModel(y:TermWorld): TermDouble = Generic(s =>
    termSum(toTermList(persons)) { TermFun(p => Times(Const(2.1), termI(Generic(sp => !termMapGet(p,y.smokes)(sp) || termMapGet(p,y.cancer)(sp)), List()))(s)}

   def f(TermWorld): TermDouble = Generic(s => term(s))

   def f(TermWorld): TermDouble = term
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

  def allStates(vars:List[Var[_]], result:List[Map[Var[_],Any]] = List(Map.empty[Var[_],Any])):List[Map[Var[_],Any]] = vars match {
    case Nil => result
    case h :: t =>
      val newMaps = for (m <- result; v <- h.dom) yield m + (h->v)
      allStates(t, newMaps)
  }

  def termToList[T](term: Term[T]): List[T] = {
    allStates(term.vars) map (m => term(new State(m)))
  }

  def termAllMaps[A, B](dom: Term[A], range: Term[B]): Term[MyMap[A, B]] = termToList(dom) match {
    case Nil => TermMyNil
    case h :: t => TermMyCons(TermTuple2(Const(h), range), termAllMaps(new Var(t), range))
  }

  def space = for (s <- allMaps(persons, bools); c <- allMaps(persons, bools)) yield World(s, c)
  def space2 = (for (s <- allMaps(persons, bools)) yield for (c <- allMaps(persons, bools)) yield World(s, c)).flatMap(identity)
  //allMaps(..) flatMap (s => allMaps(...) map (c => World(s,c))
  //allMaps(..) map (s => allMaps(...) map (c => World(s,c)) flatten
  //List(1,2,3).permutations

  def termSpace = TermWorld(termAllMaps(termPersons, termBools), termAllMaps(termPersons, termBools))

  def fg = termModel(termSpace)

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

  def termDot[K](keys: Term[List[K]])(arg1: Term[MyMap[K, Double]], arg2: Term[MyMap[K, Double]]): Term[Double] = {
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

  def map[A,B](list:List[A])(f:A=>B):List[B] = list match {
    case Nil => Nil
    case h :: t => f(h) :: map(t)(f)
  }

  def termMap[A,B](list:Term[A])(f:A=>B):Term[B] = termToList(list) match {
    case Nil => new Var(Nil)
    case h :: t => new Var(f(h) :: termToList(termMap(new Var(t))(f)))
      //termF(list):Term[B] would return a new Term such as TermPair(Const(a),list)
      //essentially: move :: into f(h)
      //say f:Term[A] => Term[B], e.g. a => TermPair(Const(b),a)
      //
      //in some cases we should return a special term termF(A)(list)
  }

  object Cross {
    def apply[T](h:T,t:Term[T]):Term[T] = ???
    def unapply[T](term:Term[T]):Option[(T, Term[T])] = ???
  }

  object EmptyTerm extends Term[Nothing] {
    def vars = Nil
    def apply(state: State) = sys.error("Cannot evaluate empty term")
  }

  def termMap2[A,B](list:Term[A])(f:A=>B):Term[B] = list match {
    case EmptyTerm => EmptyTerm
    case Cross(h,t) => Cross(f(h),termMap2(t)(f))
    case _ => termMap(list)(f)
  }

  trait Crosser[T] {
    def apply(h:T,t:Term[T]):Term[T]
    def unapply(term:Term[T]):Option[(T, Term[T])]
    def nil:Term[T]
    def isNil(term:Term[T]):Boolean
    //def vars(t:Term[T]):List[Var[_]]
  }


  def defaultCrosser[A] = new Crosser[A] {
    def apply(h: A, t: Term[A]) = new Var((h :: termToList(t)).distinct)
    def unapply(term: Term[A]) = {
      termToList(term) match {
        case Nil => None
        case h :: t => Some(h->new Var(t))
      }
    }
    def nil = new Var(Nil)
    def isNil(term: Term[A]) = termToList(term) == Nil
  }

  implicit val boolCrosser = defaultCrosser[Boolean]

  implicit def pairCrosser[A,B](implicit c1:Crosser[A],c2:Crosser[B]) = new Crosser[(A,B)] {
    def apply(h: (A, B), t: Term[(A, B)]) = t match {
      case TermTuple2(a1,a2) =>
        TermTuple2(c1(h._1,a1),c2(h._2,a2))
      case v:Var[_] =>
        new Var(h :: termToList(t))
    }
    def unapply(term: Term[(A, B)]) = term match {
      case TermTuple2(c1(h1,t1),c2(h2,t2)) => Some((h1->h2,TermTuple2(t1,t2)))
      case v:Var[_] => defaultCrosser[(A,B)].unapply(term)
    }
    def nil = TermTuple2(c1.nil,c2.nil)

    def isNil(term:Term[(A,B)]) = term match {
      case TermTuple2(a1,a2) => c1.isNil(a1) || c2.isNil(a2)
      case v:Var[_] => v.dom == Nil
    }
  }

  def termMap3[A,B](list:Term[A])(f:A=>B)(implicit crossA:Crosser[A],crossB:Crosser[B]):Term[B] = list match {
    case t if crossA.isNil(t) =>
      crossB.nil
    case crossA(h,t) =>
      crossB(f(h),termMap3(t)(f))
    case _ =>
      termMap(list)(f)
  }


  def concat[T](l1:List[T],l2:List[T]):List[T] = l1 match {
    case Nil => l2
    case h :: t => h :: concat(t,l2)
  }

  def termConcat[T](l1:Term[T],l2:Term[T]):Term[T] = termToList(l1) match {
    case Nil => l2
    case h :: t => new Var(h :: termToList[T](termConcat[T](new Var(t),l2)))
  }

  def termConcat2[T](l1:Term[T],l2:Term[T]):Term[T] = l1 match {
    case EmptyTerm => l2
    case Cross(h,t) => Cross(h,termConcat2(t,l2))
    case _ => termConcat(l1,l2)
  }
  def termConcat3[T](l1:Term[T],l2:Term[T])(implicit cross:Crosser[T]):Term[T] = l1 match {
    case t if cross.isNil(t) =>
      l2
    case cross(h,t) =>
      cross(h,termConcat3(t,l2))
    case _ =>
      termConcat(l1,l2)
  }

  def flatMap[A,B](list:List[A])(f:A => List[B]): List[B] = list match {
    case Nil => Nil
    case h :: t => concat(f(h),flatMap(t)(f))
  }

  def termFlatMap[A,B](list:Term[A])(f:A => Term[B]): Term[B] = termToList(list) match {
    case Nil => new Var(Nil)
    case h :: t => termConcat(f(h),termFlatMap(new Var(t))(f))
  }

  def termFlatMap2[A,B](list:Term[A])(f:A => Term[B]): Term[B] = list match {
    case EmptyTerm => EmptyTerm
    case Cross(h,t) => termConcat2(f(h),termFlatMap2(t)(f))
    case _ => termFlatMap(list)(f)
  }
  def termFlatMap3[A,B](list:Term[A])(f:A => Term[B])(implicit crossA:Crosser[A],crossB:Crosser[B]): Term[B] = list match {
    case t if crossA.isNil(t) => crossB.nil
    case crossA(h,t) => termConcat3(f(h),termFlatMap3(t)(f))
    case _ => termFlatMap(list)(f)
  }

  def mySpace = flatMap(bools) {a1 => map(bools) { a2 => (a1,a2)}}

  def termMySpace = termFlatMap3(termBools) { a1 => termMap3(termBools) { a2 => (a1,a2)} }

  //println(termMySpace)
  val l1 = termMap3(termBools){a => (false,a)}
  val l2 = termMap3(termBools){a => (true,a)}

  println(l1)
  println(l2)
  println(termConcat3(l1,pairCrosser[Boolean,Boolean].nil))
  println(termConcat3(l1,l2))
  //println(termMap3(termBools){a => (false,a)})

  def listMatch[T,R](t:List[T])(f1:Nil.type => R, f2: ::[T] => R) = t match {
    case Nil => f1(Nil)
    case h :: tail => f2(scala.collection.immutable.::(h,tail))
  }

  def concatFunApp[T](l1:List[T],l2:List[T]):List[T] = listMatch(l1) (
    _ => l2,
    c => c.head :: concatFunApp(c.tail,l2)
  )

}

/*

(SearchSpaceAST,ObjAST) => (FactorGraphAST) (AST that evaluates to a factor graph)

Theorem/Lemma: Let s be an AST of type List[T], and o an AST of type T => Double. Let (t,f) = convert(s,o)
where t is an AST of type Term[T] and f of type Seq[Term[Double]]. Let eval(x) of an
AST be the runtime object x evaluates to in the current scope. Then
 (i) for every element e in eval(s) there exists an assignment a(e) to the variables of eval(t), and vice versa.
 (ii) for every element e in eval(s) we have: eval(o)(e) == eval(f) map (factor => factor(a(e))) sum

Proposition: When we have an MLN in Wolfe using the style in example ..., the resulting factor graph is isomorphic to
the Ground Markov Network of the MLN.

Theoretical question: given a List AST, can you prove that it factorizes (and show how)? Cases where you can
* if the AST is a flatMap ... map structure with certain properties?
* Something more general?

For example:

  def map[A,B](list:List[A])(f:A=>B):List[B] = list match {
    case Nil => Nil //factorizes
    case h :: t => f(h) :: map(t)(f) //may temporally not factorize, or always factorize if f(h) controls only one component
  }

map(l)(f) factorizes if
* f(h) is a product?
* for all h f(h) only differs in one component (c1,..,fc(h),...)
* {f(h) | h in l} factorizes

 def concat[T](l1:List[T],l2:List[T]):List[T] = l1 match {
    case Nil => l2
    case h :: t => h :: concat(t,l2)
  }

concat(l1,l2) factorizes if
* l1 and l2 factorize and ???
* l1 and l2 are all-but-one-component constant lists, share the same non-constant component, and differ only in one constant component
* c.head :: concat(t,l2) will factorize IF c.head is the last element of l1 and the above holds for l1 and l2
* Let This means that concat(t,l2) must "almost factorize": it must contain (a,1),(a,2),(b,1) = {a,b} x {1,2} - {(b,2)}

http://santos.cis.ksu.edu/schmidt/Escuela03/WSSA/talk1p.pdf
http://www.soi.city.ac.uk/~seb/papers/thesis.ps.gz
REALLY USEFUL: http://www.researchgate.net/publication/223314158_Strictness_analysis_for_higher-order_functions/links/0deec52a9f3ed6d118000000
Abstract interpretation of list terms: N / A x B - R //meaning: nested Cartesian product with inside tree (A x B) and outside tree N
Abstract interpretation of Nil: A x B - A x B
concat(A1 x B - R1, A2 x B - R2) =  (A1 + A2) x B - (R1 + R2)
Base case:
* Nil => l2 = A2 x B2 - R2 = (A1 + A2) x B2 - A1 x B2
example
* l1 = 1 x {2,3} - 1 x {2,3}
* l2 = 2 x {2,3} - 2 x 3
* result = 2 x {2,3} - 2 x 3 = {1,2} x {2,3} - 2 x 3 - 1 x {2,3} = (A1+A2) x B - R1 - R2

Induction:
* h :: t => h :: concat(t,l2) = h :: concat(A1 x B - (R1 + {h}),A2 x B - R2) = h :: (A1+A2) x B - (R1 + h) - R2
* = (A1+A2) x B - R1 - R2

def map[F,T](list:List[F],f:F => T) = list match {
 case Nil => Nil
 case h :: t => f(h) :: map(t,f)
}

map(A, a => n/(i,a)) = N / {i} x A  //something more general?

def flatMap[F,T](list:List[F],f:F => List[T]) = list match {
 case Nil => Nil
 case h :: t => concat(f(h),flatMap(t, f))
}

flatMap(A, f: a => N / {a} x B ) = N / A x B


flatMap(Y, y => map(X, x => ((x,a),y))) =
flatMap(Y, y => (X x a) x y)





Search space:


  1. Take search space List[T]

    1. result has to evaluate to Term[T]

  2. for (a1 <- dom1; a2 <- dom2) yield ADT(a1,a2)

    1. Create isomorphic class TermADT for ADT constructor which takes Term[A] arguments instead of A arguments
    2. => TermADT(convert(dom1),convert(dom2))

  3. f(args..) (of return type  T => List[R])

    1. add termf definition


      1. Arguments types for different original types

        1. arguments of type A

          1. stay the same

        2. arguments of type List[A]

          1. replaced by Term[A] //or something more concrete?

            1. or TermA?


        3. each argument of structured type Function1[A,List[B]]

      2. Return type

        1. TermR?
        2. Term[R]?

      3. new Body = convert(body with arguments replaced by termToList(arg))


  4. new Var(termToList(arg) => arg
  5. All other scala expressions are converted in the trivial way

    1. for example, "case pat => result" becomes "case pat => convert(body)”
    2. if (cond) r1 else r2

      1. if (cond) convert(r1) else convert(r2)



funConvert method: Args => Double:


  1. input: arguments => Double

    1. for each argument of type A,

      1. create TermA (if needed)
      2. replace type A with TermA

    2. replace body with newBody = Generic(s => body where each arg is replaced by arg(s))


  2. convert(newBody) into Term[Double]


Double AST to Term[Double] AST


   * arg1 + arg2 => Plus(convert(arg1),convert(arg2))
   * arg1 * arg2 => Times(convert(arg1),convert(arg2))
   * f(arg) => {funConvert(f); termf(simplify(toStructuredConst(arg))) }

      * by default Const(arg)


         * or better toStructuredConst(arg)

            * this method exists for each ADT and creates a TermADT



   *



 */