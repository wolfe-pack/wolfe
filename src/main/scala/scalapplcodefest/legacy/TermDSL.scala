package scalapplcodefest.legacy

import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scalapplcodefest.legacy.value._
import scalapplcodefest.legacy.term._
import scalapplcodefest.legacy.value.RangeSet
import scala.Some
import scalapplcodefest.legacy.term.RestrictedFun
import scalapplcodefest.legacy.term.DynFunTerm
import scalapplcodefest.legacy.value.SeqSet
import scalapplcodefest.legacy.term.FunApp
import cc.factorie.la.SingletonTensor1
import cc.factorie.WeightsSet
import cc.factorie.optimize.{Perceptron, OnlineTrainer, Trainer}
import scalapplcodefest.legacy._
import scalapplcodefest.Index
import scalapplcodefest._

/**
 * This object provides a set of implicit conversions that allow users
 * to write down terms more compactly.
 *
 * @author Sebastian Riedel
 */
object TermDSL extends ValueDSL {

  implicit def intToConstant(x: Int) = Constant(x)
  implicit def intToTerm(x: Int) = RichIntTerm(x)
  implicit def doubleToConstant(x: Double) = Constant(x)
  implicit def booleanToConstant(x: Boolean) = Constant(x)
  implicit def symbolToConstant(x: Symbol) = Constant(x)
  implicit def stringToConstant(x: String) = Constant(x)
  //implicit def predicateToAllAtoms[A,B](p:Predicate[A,B]) = AllGroundAtoms(p)

  //implicit def setToConstant[T](x: Set[T]) = Constant(x)
  //implicit def setToRichSetTerm[T](x: Set[T]) = RichSetTerm(Constant(x))

  //implicit def funToConstant[A,B](x:Fun[A,B]) = x)
  implicit def toTupleTerm2[T1, T2](tuple: (Term[T1], Term[T2])) = TupleTerm2(tuple._1, tuple._2)
  //implicit def toTupleTerm3[T1, T2](tuple: (T1,T2)) = TupleTerm2(Constant(tuple._1), Constant(tuple._2))
  implicit def toRichTupleTerm2[T1, T2](tuple: (Term[(T1, T2)])) = RichTupleTerm2(tuple)
  implicit def toRichVariable[T](v: Variable[T]) = RichVariable(v)
  implicit def toRichTerm[T](term: Term[T]) = new RichTerm(term)
  implicit def toRichInt[A](i: Term[Int]) = RichIntTerm(i)
  implicit def toRichDouble[A](t: Term[Double]) = RichDoubleTerm(t)
  implicit def toRichBooleanTerm(t: Term[Boolean]) = RichBooleanTerm(t)

  implicit def toRichFunTerm[A, B](term: Term[Fun[A, B]]): RichFunTerm[A, B] = RichFunTerm(term)
  implicit def toRichFunctionSeq[A, B](f: FunTerm[Seq[A], B]) = RichFunctionTermSeq(f)
  implicit def toRichFunction2[A1, A2, B](f: Term[Fun[(A1, A2), B]]) = RichFunctionTerm2(f)
  implicit def toRichFunction3[A1, A2, A3, B](f: Term[Fun[(A1, A2, A3), B]]) = RichFunctionTerm3(f)
  implicit def toRichPredicate[A, B](p: Predicate[A, B]) = RichPredicate(p)
  implicit def toRichPredicate2[A1, A2, B](p: Predicate[(A1, A2), B]) = RichPredicate2(p)
  implicit def toRichCartesianProductTerm2[T1, T2](term: CartesianProductTerm2[T1, T2]) = RichCartesianProductTerm2(term)
  implicit def toRichCartesianProductTerm3[T1, T2, T3](term: CartesianProductTerm3[T1, T2, T3]) = RichCartesianProductTerm3(term)
  implicit def toRichSetTerm[T](s: Term[Set[T]]) = RichSetTerm(s)
  implicit def toRichVec(term: Term[Vector]) = RichVecTerm(term)
  implicit def toRichVarSymbol(symbol: Symbol) = RichVarSymbol(symbol)
  implicit def toRichPredSymbol(symbol: Symbol) = RichPredSymbol(symbol)

  implicit def toRichIndex(index: Index) = RichIndex(index)

  case class Assign[T](variable: Variable[T], value: T)

  implicit def toAssign[T](pair: (Variable[T], T)) = Assign(pair._1, pair._2)


  def state(assignments: Assign[_]*) =
    if (assignments.isEmpty) State.empty else State(assignments.map(a => a.variable -> a.value).toMap)

//  def state[T](map: Map[Variable[T],T]) = State(map.asInstanceOf[Map[Variable[Any],Any]])

  def funTerm[A, B](f: PartialFunction[A, B]) = Constant(value.Fun(f, new AllOfType[A], new AllOfType[B]))

  //create bracketed terms
  def br[T](term: Term[T]) = Bracketed(term)

  //table building
  def T[A, B](domain: Term[Set[A]], f: PartialFunction[A, B]) = Constant(value.Fun.table(domain.eval().get, f))

  //math
  def unit(index: Term[Int], value: Term[Double] = Constant(1.0)) = FunApp(vectors.unit, TupleTerm2(index, value))
  def I(term: Term[Boolean]) = FunApp(bools.iverson, term)
  def log(term: Term[Double]) = FunApp(doubles.log, term)

  implicit def toImageSeq[A, B](f: FunTerm[A, B]) = ImageSeq1(f)
  implicit def toImageSeqCurried2[A1, A2, B](f: FunTerm[A1, Fun[A2, B]]) = ImageSeq2(f)


  implicit def uncurry[A1, A2, R](f: FunTerm[A1, Fun[A2, R]]) = f match {
    case Curried2(uncurried) => uncurried
    case _ => ???
  }

  case class RichVarSymbol(symbol: Symbol) {
    def of[T](set: Term[Set[T]]) = Var(symbol, set)
    //def of[T](set: Set[T]) = Var(symbol, Constant(set))
  }

  case class RichState(state: State) {
    def +[T](pair: (Variable[T], T)) = state + SingletonState(pair._1, pair._2)
  }

  case class RichPredSymbol(symbol: Symbol) {
    def of[A, B](domRange: (Term[Set[A]], Term[Set[B]])) = Predicate(symbol, domRange._1, domRange._2)
  }

  case class RichVariable[T](v: Variable[T]) {
    //def ->(value:T) = VarValuePair(v,value)
  }

  case class RichVecTerm(term: Term[Vector]) {
    def dot(that: Term[Vector]) = FunApp(vectors.dot, TupleTerm2(term, that))
    def +(that: Term[Vector]) = FunApp(vectors.add, TupleTerm2(term, that))
    def -(that: Term[Vector]) = FunApp(vectors.minus, TupleTerm2(term, that))
    //rockt: needs to return a vector!
    //def *(that: Term[Double]): FunApp[(Vector, Any), Vector] = ??? //TODO
    
    def *(that: Term[Double]) = FunApp(vectors.scalarTimes, TupleTerm2(term, that))
  }

  private var anonVarCount = 0
  def freshName() = {
    this.synchronized {
      anonVarCount += 1
    }
    "_x" + anonVarCount
  }

  case class RichSetTerm[T](s: Term[Set[T]], variableName: () => String = () => freshName()) {

    def freshVariable[A](dom: Term[Set[A]] = s) = Var(Symbol(variableName()), dom)

    def as(name: String) = RichSetTerm(s, () => name)
    def as(symbol: Symbol) = RichSetTerm(s, () => symbol.name)


    def map[R](f: Variable[T] => Term[R]): LambdaAbstraction[T, R] = {
      val variable = freshVariable()
      LambdaAbstraction(VarSig(variable), f(variable))
    }
    def flatMap[A1, A2](f: Variable[T] => LambdaAbstraction[A1, A2]) = {
      val variable: Variable[T] = freshVariable()
      val innerLambda = f(variable)
      LambdaAbstraction(VarSig(variable), innerLambda)
    }


    def |->[T2](that: Term[Set[T2]]) = (s, that)
    def ||->[T2](that: Term[Set[T2]]) = AllFunctionsTerm(s,that) //FunApp(Constant(new AllFunctionsOp[T, T2]), TupleTerm2(s, that))


    def mappedBy[A](f: FunTerm[T, A]) =
      FunApp(RestrictedFun(MapIterable, CartesianProductTerm2(s.domain, f.domain), Constant(new AllOfType[Set[A]])), (s, f))

    def collectedBy[A](f: FunTerm[T, A]) =
      FunApp(RestrictedFun(CollectIterable, CartesianProductTerm2(s.domain, f.domain), Constant(new AllOfType[Set[A]])), (s, f))

    def filteredBy[A](f: FunTerm[T, Boolean]) =
      FunApp(RestrictedFun(FilterIterable, CartesianProductTerm2(s.domain, f.domain), Constant(new AllOfType[Set[T]])), (s, f))

  }

  case class RichCartesianProductTerm2[T1, T2](term: CartesianProductTerm2[T1, T2],
                                               variableName1: () => String = () => freshName(),
                                               variableName2: () => String = () => freshName()) {
    def as(name1: String, name2: String) = RichCartesianProductTerm2(term, () => name1, () => name2)
    def as(symbol1: Symbol, symbol2: Symbol) = RichCartesianProductTerm2(term, () => symbol1.name, () => symbol2.name)

    def map[R](f: ((Variable[T1], Variable[T2])) => Term[R]): LambdaAbstraction[(T1, T2), R] = {
      val variable1 = Var(Symbol(variableName1()), term.a1)
      val variable2 = Var(Symbol(variableName2()), term.a2)
      val applied = f(variable1, variable2)
      LambdaAbstraction(TupleSig2(VarSig(variable1), VarSig(variable2)), applied)
    }

    def filter(f: ((Variable[T1], Variable[T2])) => Boolean) = this
    def withFilter(f: ((Variable[T1], Variable[T2])) => Boolean) = this
  }

  case class RichCartesianProductTerm3[T1, T2, T3](term: CartesianProductTerm3[T1, T2, T3],
                                               variableName1: () => String = () => freshName(),
                                               variableName2: () => String = () => freshName(),
                                               variableName3: () => String = () => freshName()) {
    def as(name1: String, name2: String, name3: String) = RichCartesianProductTerm3(term, () => name1, () => name2, () => name3)
    def as(symbol1: Symbol, symbol2: Symbol, symbol3: Symbol) =
      RichCartesianProductTerm3(term, () => symbol1.name, () => symbol2.name, () => symbol3.name)

    def map[R](f: ((Variable[T1], Variable[T2], Variable[T3])) => Term[R]): LambdaAbstraction[(T1, T2, T3), R] = {
      val variable1 = Var(Symbol(variableName1()), term.a1)
      val variable2 = Var(Symbol(variableName2()), term.a2)
      val variable3 = Var(Symbol(variableName3()), term.a3)
      val applied = f(variable1, variable2, variable3)
      LambdaAbstraction(TupleSig3(VarSig(variable1), VarSig(variable2), VarSig(variable3)), applied)
    }

    def filter(f: ((Variable[T1], Variable[T2], Variable[T3])) => Boolean) = this
    def withFilter(f: ((Variable[T1], Variable[T2], Variable[T3])) => Boolean) = this
  }


  case class VarValuePair[T](variable: Variable[T], value: T)

  class RichTerm[T](term: Term[T]) {
    def |(condition: State) = Conditioned(term, condition)
    def |(mappings: (Variable[Any], Any)*) = Conditioned(term, State(mappings.toMap))
    def eval(state: (Variable[Any], Any)*) = term.eval(State(state.toMap))
    def value(state: (Variable[Any], Any)*) = term.value(State(state.toMap))
    def ===(that: Term[T]) = FunApp(RestrictedFun[(T, T), Boolean](Equal), TupleTerm2(term, that))
    //FunApp(new Equals[T].Term, TupleTerm2(term, that))
    def ===(that: T) = FunApp(RestrictedFun[(T, T), Boolean](Equal), TupleTerm2(term, Constant(that))) //FunApp(new Equals[T].Term, TupleTerm2(term, Constant(that)))
    //    def eval(state:VarValuePair[T]*):Option[T] = term.eval(State(state.map(_.toTuple).toMap))
    def hint(hint:CompilerHint) = Annotation(term,hint)

  }

  case class RichTupleTerm2[T1, T2](t: Term[(T1, T2)]) {
    def _1 = ArgOf(t, Constant(0))
    def _2 = ArgOf(t, Constant(1))
  }

  case class RichIntTerm(i: Term[Int]) {
    def +(that: Term[Int]) = FunApp(ints.add, TupleTerm2(i, that))
    def -(that: Term[Int]) = FunApp(ints.minus, TupleTerm2(i, that))
    def /(that: Term[Int]) = FunApp(ints.divide, TupleTerm2(i, that))
    def *(that: Term[Int]) = FunApp(ints.times, TupleTerm2(i, that))

    def ~~(that: Term[Int]) = RangeSet(i, that) // FunApp(ints.range, TupleTerm2(i, that)) doesn't work because funapp can't guess reasonable default
  }

  case class RichDoubleTerm(x: Term[Double]) {
    def +(that: Term[Double]) = FunApp(doubles.add, TupleTerm2(x, that))
    def -(that: Term[Double]) = FunApp(doubles.minus, TupleTerm2(x, that))
    def *(that: Term[Double]) = FunApp(doubles.times, TupleTerm2(x, that))
  }

  case class RichBooleanTerm(x: Term[Boolean]) {
    def &&(that: Term[Boolean]) = FunApp(bools.and, TupleTerm2(x, that))
    def ||(that: Term[Boolean]) = FunApp(bools.or, TupleTerm2(x, that))
    def |=>(that: Term[Boolean]) = FunApp(bools.implies, TupleTerm2(x, that))
    def <=>(that: Term[Boolean]) = FunApp(bools.equiv, TupleTerm2(x, that))
    def unary_! = FunApp(bools.neg, x)
    def unary_$ = FunApp(bools.iverson, x)
  }


  case class RichFunTerm[A, B](f: Term[Fun[A, B]]) {
    val FunTerm(funCandidateDom, _) = f
    def apply(a: Term[A]) = FunApp(f, a)
    def isDefined = for (x <- funCandidateDom) yield
      FunApp(RestrictedFun(IsDefined, CartesianProductTerm2(f.domain, funCandidateDom), Constant(Bools)), TupleTerm2(f, x))

  }

  case class RichFunctionTerm2[A1, A2, B](f: Term[Fun[(A1, A2), B]]) {
    def apply(a1: Term[A1], a2: Term[A2]) = FunApp(f, TupleTerm2(a1, a2))
    def apply(a1: Term[(A1, A2)]) = FunApp(f, a1)
  }

  case class RichFunctionTerm3[A1, A2, A3, B](f: Term[Fun[(A1, A2, A3), B]]) {
    def apply(a1: Term[A1], a2: Term[A2], a3: Term[A3]) = FunApp(f, TupleTerm3(a1, a2, a3))
    def apply(a1: Term[(A1, A2, A3)]) = FunApp(f, a1)
  }

  case class RichFunctionTermSeq[A, B](f: FunTerm[Seq[A], B]) {
    def apply[C](args: Term[A]*)(implicit convert: C => Term[A]) = FunApp(f, SeqTerm(args.toSeq))
  }


  case class RichPredicate[A, B](p: Predicate[A, B]) {
    def atom(a: A) = GroundAtom(p, a)
    def allAtoms = AllGroundAtoms(p)
  }

  case class RichPredicate2[A1, A2, B](p: Predicate[(A1, A2), B]) {
    def atom(a1: A1, a2: A2) = GroundAtom(p, (a1, a2))
  }

  case class RichIndex(index: Index) {
    def apply(key: Symbol) = RichIndexFunction(index, key)
  }

  case class RichIndexFunction(index: Index, symbol: Symbol) {
    def apply[A1 <: AnyRef](a1: Term[A1]) =
      dynFun[A1, Int]({case x => index.index(Array(symbol, x))}, a1.domain, ints)(a1)
    def apply[A1 <: AnyRef, A2 <: AnyRef](a1: Term[A1], a2: Term[A2]) =
      dynFun[(A1, A2), Int]({case (x1, x2) => index.index(Array(symbol, x1, x2))}, c(a1.domain, a2.domain), ints)(a1, a2)
  }

  //}

  trait ConstantValue[T] extends Term[T] {
    def unapply(term: Term[Any]): Boolean = term == this
  }

  trait ConstantFun1[A, B] extends ConstantValue[Fun[A, B]] {
    self =>

    object Applied1 {
      def unapply(term: Term[Any]): Option[Term[A]] = term match {
        case FunApp(op, arg) if op eq self => Some(arg.asInstanceOf[Term[A]])
        case _ => None
      }
    }

  }

  trait ConstantFun2[A1, A2, B] extends ConstantFun1[(A1, A2), B] {
    self =>

    object Applied2 {
      def unapply(term: Term[Any]): Option[(Term[A1], Term[A2])] = term match {
        case FunApp(op, TupleTerm2(arg1, arg2)) if self == op => Some(arg1.asInstanceOf[Term[A1]], arg2.asInstanceOf[Term[A2]])
        case _ => None
      }
    }

  }

  trait ConstantOperator[T] extends ConstantFun2[T, T, T] {
    self =>

    object Reduced {
      def unapply(x: Term[Any]): Option[Term[Seq[T]]] = x match {
        case Reduce(op, args) if op == self => Some(args.asInstanceOf[Term[Seq[T]]])
        case _ => None
      }
    }

    def reduce(args: Term[Seq[T]]) = Reduce(self, args)

  }


  trait ConstantSet[T] extends ConstantValue[Set[T]] {
    this: Term[Set[T]] =>
    val equal = RestrictedFun(Equal, CartesianProductTerm2(this, this), bools)

  }

  trait HasAdd[T] {
    def add: ConstantOperator[T]
    def sum(args: Term[T]*) = Reduce(add, SeqTerm(args))
    def sumSeq(args: Seq[Term[T]]) = Reduce(add, SeqTerm(args))
    def sumSeq(args: Term[Seq[T]]) = Reduce(add, args)
    def sum[A](args: Term[Fun[A, T]]) = Reduce(add, ImageSeq1(args))
  }

  object ints extends Constant(Ints) with ConstantSet[Int] with HasAdd[Int] {
    val add = new Constant(Ints.Add) with ConstantOperator[Int]
    val minus = new Constant(Ints.Minus) with ConstantOperator[Int]
    val range = new Constant(Ints.Range) with ConstantFun2[Int, Int, Set[Int]]
    val divide = new Constant(Ints.Divide) with ConstantOperator[Int]
    val times = new Constant(Ints.Times) with ConstantOperator[Int]

  }

  object doubles extends Constant(Doubles) with ConstantSet[Double] with HasAdd[Double] {
    val add = new Constant(Doubles.Add) with ConstantOperator[Double]
    val minus = new Constant(Doubles.Minus) with ConstantOperator[Double]
    val times = new Constant(Doubles.Times) with ConstantOperator[Double]
    val log = new Constant(Doubles.Log) with ConstantFun1[Double, Double]
  }

  object bools extends Constant(Bools) with ConstantSet[Boolean] {
    val and = new Constant(Bools.And) with ConstantOperator[Boolean]
    val or = new Constant(Bools.Or) with ConstantOperator[Boolean]
    val implies = new Constant(Bools.Implies) with ConstantOperator[Boolean]
    val equiv = new Constant(Bools.Equiv) with ConstantOperator[Boolean]
    val neg = new Constant(Bools.Neg) with ConstantFun1[Boolean, Boolean]
    val iverson = new Constant(Bools.Iverson) with ConstantFun1[Boolean, Double]

  }

  object vectors extends Constant(Vectors) with ConstantSet[Vector] with HasAdd[Vector] {
    val dot = new Constant(Vectors.Dot) with ConstantFun2[Vector, Vector, Double]
    val add = new Constant(Vectors.VecAdd) with ConstantOperator[Vector]
    val minus = new Constant(Vectors.VecMinus) with ConstantOperator[Vector]
    val unit = new Constant(Vectors.UnitVector) with ConstantFun2[Int, Double, Vector]
    val scalarTimes = new Constant(Vectors.ScalarTimes) with ConstantFun2[Vector, Double, Vector]
  }

  object strings extends Constant(Strings) with ConstantSet[String] {
    val length = fun[String, Int]({case x => x.length})
  }

  //val all = new Constant(All) with ConstantSet[Any] {}

  def c[T1, T2](arg1: Term[Set[T1]], arg2: Term[Set[T2]]) = CartesianProductTerm2(arg1, arg2)
  def c[T1, T2, T3](arg1: Term[Set[T1]], arg2: Term[Set[T2]], arg3: Term[Set[T3]]) = CartesianProductTerm3(arg1, arg2, arg3)
  def all[T] = Constant(new AllOfType[T])
  def set[T](values: T*) = Constant(SeqSet(values))
  def seq[T](values: Term[T]*) = SeqTerm(values)
  def tuple[T1,T2](arg1:Term[T1],arg2:Term[T2]) = TupleTerm2(arg1,arg2)


  def fun[A, B](f: PartialFunction[A, B], dom: Set[A] = new AllOfType[A], range: Set[B] = new AllOfType[B]): Term[Fun[A, B]] =
    Constant(value.Fun(f, dom, range))
  //def fun[A,B](pairs:(A,B)*):Term[Fun[A,B]] = Constant(Fun(pairs.toMap))


  def dynFun[A, B](f: PartialFunction[A, B], dom: Term[Set[A]] = all[Set[A]], range: Term[Set[B]] = all[Set[B]]) = DynFunTerm(f, dom, range)

  def max[T](f:LambdaAbstraction[T,Double]) = RichMax(f)
  def min(f:LambdaAbstraction[Vector,Double]) = RichMin(f)

  def logZ[T](f:LambdaAbstraction[T,Double]) = RichLogZ(f)

  def arg[T](max:Max[T]) = max.argmax
  def arg[T](helper:MinHelper[T]) = helper.argmin
  def argState[T](max:Max[T]) = max.argmaxState
  def argState[T](max:MinHelper[T]) = max.argminState

  case class RichMin(f:LambdaAbstraction[Vector,Double]) {
    def byTrainer(trainerFor: WeightsSet => Trainer = new OnlineTrainer(_, new Perceptron, 5)) = {
      f match {
        case LambdaAbstraction(VarSig(param),objective) if param.domain == vectors =>
          val learned = TrainerBasedMaximization.minimize(param.asInstanceOf[Variable[Vector]],objective,trainerFor)
          MinHelper(
            Term[Vector]( state => learned, Set.empty, param.default),
            StateTerm(state => State(Map(param -> learned)),Set.empty))
        case _ => sys.error("We only support gradient-based minimization for vector arguments")
      }
    }
  }
  
  case class RichMax[T](f:LambdaAbstraction[T,Double]){
    def byBruteForce = Max.ByBruteForce(f)
    def byMessagePassing(algorithm: MPGraph => Unit = MaxProduct.apply(_, 1)) = Max.ByMessagePassing(f,algorithm)
  }

  case class RichLogZ[T](f:LambdaAbstraction[T,Double]){
    def byBruteForce = LogZ.ByBruteForce(f)
  }

  case class MinHelper[T](argmin:Term[T], argminState:Term[State])

  implicit def toSig[T](variable: Variable[T]) = VarSig(variable)
  implicit def toSig[A,B](predicate: Predicate[A,B]) = PredSig(predicate)

  def sig[T1, T2](sig1: Sig[T1], sig2: Sig[T2]) = TupleSig2(sig1, sig2)
  def sig[T1, T2, T3](sig1: Sig[T1], sig2: Sig[T2], sig3:Sig[T3]) = TupleSig3(sig1, sig2,sig3)
  def lam[A, B](sig: Sig[A], body: Term[B]) = LambdaAbstraction(sig, body)


  def argmax[T](f: Term[Fun[T, Double]]) = FunApp(RestrictedFun(Argmax,all[Fun[T,Double]],all[T]), f)
  def argmin[T](f: Term[Fun[T, Double]]) = FunApp(RestrictedFun(Argmin,all[Fun[T,Double]],all[T]), f)
  def maxExperimental[T](f:Term[Fun[T,Double]]) = FunApp(RestrictedFun(MaxValue,all[Fun[T,Double]],doubles), f)
  def minExperimental[T](f:Term[Fun[T,Double]]) = FunApp(RestrictedFun(MinValue,all[Fun[T,Double]],doubles), f)

}

trait ValueDSL {
  def Fun[A, B](map: Map[A, B]): Fun[A, B] = value.Fun(map, map.keySet, map.values.toSet)
  def Table[A, B](pairs: (A, B)*): Fun[A, B] = Fun(pairs.toMap)
  def Tab[A, B](domain: Set[A], range: Set[B], fun: PartialFunction[A, B]) = new Fun[A, B] with DeepEqualFun {
    def apply(v1: A) =
      if (fun.isDefinedAt(v1))
        fun(v1)
      else
        range.head
    override def funDom = domain
    def funCandidateDom = domain
    def funRange = range
    def isDefinedAt(x: A) = domain(x)
  }
  def Unit(index: Int, value: Double = 1.0) = new SingletonTensor1(1, index, value)
}