package ml.wolfe.term

import cc.factorie.Factorie.DenseTensor1
import cc.factorie.la.{DenseTensor2, GrowableSparseTensor1}
import ml.wolfe.{Index, Mat, Vect}

import scala.language.implicitConversions
import scala.util.Random

/**
 * @author riedel
 */
object TermImplicits extends NameProviderImplicits with MathImplicits with Stochastic with LoggedTerms with FVectors with NGramCountsHelper {

  implicit val Doubles: Dom.doubles.type = Dom.doubles
  implicit val Bools: Dom.bools.type = Dom.bools
  implicit val Ints: Dom.ints.type = Dom.ints

  def Discretes[T](args: T*) = new DiscreteDom[T](args.toIndexedSeq)

  def Ints[T](args: Range) = new RangeDom(args)

  def Pairs(d1: Dom, d2: Dom) = new Tuple2Dom[d1.type, d2.type](d1, d2)

  implicit def rangeToDom(range: Range): RangeDom = new RangeDom(range)

  implicit def domToIterable(dom: Dom): Iterable[dom.Value] = dom.toIterable

  //def >[T](term:TypedTerm[T]):T = term.eval()

  def Seqs[D <: Dom](elements: D, minLength: Int, maxLength: Int): VarSeqDom[elements.type] =
    new VarSeqDom[elements.type](elements, maxLength, minLength)

  def Seqs[D <: Dom](elements: D, length: Int): VarSeqDom[elements.type] = Seqs(elements, length, length)

  def Maps(keyDom: Dom, valueDom: Dom): MapDom1[keyDom.type, valueDom.type] = new MapDom1[keyDom.type, valueDom.type](keyDom, valueDom)

  def Maps(keyDom1: Dom, keyDom2: Dom, valueDom: Dom): MapDom2[keyDom1.type, keyDom2.type, valueDom.type] =
    new MapDom2[keyDom1.type, keyDom2.type, valueDom.type](keyDom1, keyDom2, valueDom)

  def Preds(keyDom: Dom) = Maps(keyDom, Bools)

  def Graphs(keyDom1: Dom, keyDom2: Dom) = Maps(keyDom1,keyDom2,Bools)

  def fixedLengthSeq[T](elements: Seq[T])(implicit dom: TypedDom[T]) = {
    Seqs(dom, elements.length).Const(elements.toIndexedSeq)
  }

  def SeqConst[T](elements: T*)(implicit dom: TypedDom[T]) = {
    fixedLengthSeq[T](elements)
  }

  def mem[T <: Term[Dom]](term: T) = term.domain.own(new Memoized[Dom, T](term).asInstanceOf[TypedTerm[term.domain.Value]])

  def cached[T <: Term[Dom]](keys: AnyTerm*)(term: T) =
    term.domain.own(new CachedTerm[Dom, T](keys: _*)(term).asInstanceOf[TypedTerm[term.domain.Value]])


  def choice[T <: Term[Dom]](index: IntTerm)(choice1: T, choices: T*): choice1.domain.Term =
    choice1.domain.own(SeqTerm(choice1 +: choices: _*)(index).asInstanceOf[TypedTerm[choice1.domain.Value]])

  def ifThenElse[T <: Term[Dom]](cond: BoolTerm)(ifTrue: T)(ifFalse: T) =
    choice(boolToInt(cond))(ifFalse, ifTrue)

  implicit def indexerToIndex(indexer: DefaultIndexer): Index = indexer.index

  def indexed(value: AnyTerm)(implicit indexer: Indexer) = Indexed(value)

  //  implicit class ConvertableToTerm[T, D <: TypedDom[T]](value: T)(implicit val domain: D) {
  //    def toConst: domain.Term = domain.Const(value)
  //  }
  //  implicit class ConvertableToTerm2[T](value: T)(implicit val domain: TypedDom[T]) {
  //    def toConst2: domain.Term = domain.Const(value)
  //  }

  class ConvertableToTerm3[T, D <: TypedDom[T]](value: T)(implicit val domain: D) {
    def toConst: domain.Term = domain.Const(value)
  }

  def fun[D <: Dom](d: Dom)(f: d.Term => Term[D]): d.Value => D#Value = {
    val v = d.Variable("v")
    val t = f(v)
    val e = t.evaluator()
    (x: d.Value) => e.evalUntyped(x)
  }

  def fun[D <: Dom](d1: Dom, d2: Dom)(f: (d1.Term, d2.Term) => Term[D]): (d1.Value, d2.Value) => D#Value = {
    val v1 = d1.Variable("v1")
    val v2 = d2.Variable("v2")
    val t = f(v1, v2)
    val e = t.evaluator()
    (a1: d1.Value, a2: d2.Value) => e.evalUntyped(a1, a2)
  }

  implicit def pairToPairTerm[T1 <: AnyTerm, T2 <: AnyTerm](pair: (T1, T2)): Tuple2Dom[pair._1.domain.type, pair._2.domain.type]#Term = {
    val dom = new Tuple2Dom[pair._1.domain.type, pair._2.domain.type](pair._1.domain, pair._2.domain)
    dom.Term(pair._1.asInstanceOf[dom.dom1.Term], pair._2.asInstanceOf[dom.dom2.Term])
  }


  implicit def toConvertable[T](value: T)(implicit domain: TypedDom[T]): ConvertableToTerm3[T, domain.type] = new ConvertableToTerm3[T, domain.type](value)(domain)

  implicit def evalResultToResult[T](evalResult:EvalResult[T]):T = evalResult.value

  implicit class RichRange(values: Range) {
    def toDom = new RangeDom(values)
  }

  implicit class RichSeq[T](values: Seq[T]) {
    def toDom = new DiscreteDom[T](values.toIndexedSeq)

    def toConst(implicit dom: TypedDom[T]) = fixedLengthSeq[T](values.toSeq)
  }

  class RichSeqTerm[E <: Dom, S <: SeqTerm[E]](val seq: S) {
    def slice(from: IntTerm, to: IntTerm)(implicit sliceDom: VarSeqDom[E]): sliceDom.Term = {
      val result = new VarSeqSlice[Dom, SeqTerm[E], VarSeqDom[E]](seq, from, to)(sliceDom)
      sliceDom.own(result.asInstanceOf[TypedTerm[sliceDom.Value]])
    }

    def takeRight(num: IntTerm)(implicit sliceDom: VarSeqDom[E]) = slice(seq.length - num, seq.length)

    def take(num: IntTerm)(implicit sliceDom: VarSeqDom[E]) = slice(0, num)

    def drop(num: IntTerm)(implicit sliceDom: VarSeqDom[E]) = slice(num, seq.length)

    def dropRight(num: IntTerm)(implicit sliceDom: VarSeqDom[E]) = slice(0, seq.length - num)


    def append(elem: seq.domain.elementDom.Term)(implicit appendedDom: VarSeqDom[seq.domain.elementDom.type]): appendedDom.Term = {
      val result = new VarSeqAppend[Dom, SeqTerm[E], appendedDom.type](seq, elem)(appendedDom)
      appendedDom.own(result.asInstanceOf[TypedTerm[appendedDom.Value]])
    }

    def :+(elem: seq.domain.elementDom.Term)(implicit appendedDom: VarSeqDom[seq.domain.elementDom.type]) =
      append(elem)(appendedDom)

  }

  //  implicit def toRichSeqTerm(seq:AnySeqTerm):RichSeqTerm[seq.domain.elementDom.type,seq.domain.type] =
  //    new RichSeqTerm[seq.domain.elementDom.type,seq.domain.type](seq.asInstanceOf[seq.domain.Term])

  //  implicit def toRichSeqTerm[S <: VarSeqDom[_]](seq:S#Term):RichSeqTerm[seq.domain.elementDom.type,S] =
  //    new RichSeqTerm[seq.domain.elementDom.type,S](seq.asInstanceOf[seq.domain.Term])

  implicit def toRichSeqTerm[E <: Dom](seq: SeqTerm[E]): RichSeqTerm[E, seq.type] =
    new RichSeqTerm[E, seq.type](seq)

  //  implicit def genericToConstant[T,D<:TypedDom[T]](t:T)(implicit dom:D):dom.Term = dom.const(t)
  //  implicit def genericToConstant[T,D<:TypedDom[T]](t:T)(implicit dom:D):dom.DomTerm = dom.const(t)

  //  implicit def seqOfTermsToSeqTerm[T <: Term[Dom], D <: DomWithTerm[T],S<:SeqDom[D]](seq:IndexedSeq[T])(implicit dom:S):dom.Term =
  //    new dom.SeqTermImpl {
  //      val elements:IndexedSeq[T] = seq
  //      val domain:dom.type = dom
  //    }

  def SeqTerm[D <: Dom](args: Term[D]*): VarSeqDom[D]#Term = VarSeq[D](Ints.Const(args.length), args.toIndexedSeq)

  def SeqTerm[D <: Dom](length: IntTerm)(args: Term[D]*): VarSeqDom[D]#Term = VarSeq[D](length, args.toIndexedSeq)

  def VarSeq[D <: Dom](length: IntTerm, args: IndexedSeq[Term[D]]): VarSeqDom[D]#Term = {
    val dom = Seqs[D](args.head.domain, args.length)
    dom.Term(length, args.asInstanceOf[IndexedSeq[dom.elementDom.Term]])
    //    new VarSeqConstructor[D](length, args)
  }


  //implicit def seqToConstant[T,D<:TypedDom[T]](seq:IndexedSeq[T])(implicit dom:SeqDom[D]):dom.TermType = dom.const(seq)

  //implicit def seqToSeqTerm[E <: Dom : SeqDom](elems:Seq[Term[E]]) = seq(implicitly[SeqDom[E]])(elems: _*)


  implicit def discToConstant[T: DiscreteDom](value: T): Constant[DiscreteDom[T]] =
    implicitly[DiscreteDom[T]].Const(value)


  //  def argmax[D <: Dom](dom: D)(obj: dom.Variable => DoubleTerm): dom.Value = {
  //    val variable = dom.variable("_hidden")
  //    val term = obj(variable)
  //    term.argmax(variable).asInstanceOf[dom.Value]
  //  }

  class RichMapTerm2[+M <: MapTerm[Tuple2Dom[_ <: Dom, _ <: Dom], _ <: Dom]](val term: M) {
    def apply(k1: term.domain.keyDom.dom1.Term, k2: term.domain.keyDom.dom2.Term): term.domain.valueDom.Term = {
      term.asInstanceOf[term.domain.Term](term.domain.keyDom.Term(k1, k2))
    }

  }

  class RichMapTerm2New[T1 <: Dom, T2 <: Dom, V <: Dom, M <: MapTerm[Tuple2Dom[T1, T2], V]](val term: M) {
    def apply(k1: term.domain.keyDom.dom1.Term, k2: term.domain.keyDom.dom2.Term): term.domain.valueDom.Term = {
      term.asInstanceOf[term.domain.Term](term.domain.keyDom.Term(k1, k2))
    }
  }

  implicit def toRichMapTerm2(m: MapTerm[Tuple2Dom[_ <: Dom, _ <: Dom], _ <: Dom]): RichMapTerm2[m.type] =
    new RichMapTerm2[m.type](m)

  //  implicit def toRichMapTerm2New[T1 <:Dom, T2<:Dom, V <: Dom, M <: MapTerm[Tuple2Dom[T1,T2],V]](m:M):RichMapTerm2New[T1,T2,V,M] =
  //    new RichMapTerm2New[T1,T2,V,M](m)


  implicit class RichBoolTerm(term: BoolTerm) {
    def &&(that: BoolTerm) = new And(term, that)

    def ||(that: BoolTerm) = new Or(term, that)

    def -->(that: BoolTerm) = new Implies(term, that)

    //def unary_olf = for (b <- term) yield !b todo: OLF
    def unary_! = Dom.bools.own(new Unary_!(term))

    def <->(that: BoolTerm) = new DiscreteEquals[Boolean](
      term.asInstanceOf[Term[GenericDiscreteDom[Boolean]]],
      that.asInstanceOf[Term[GenericDiscreteDom[Boolean]]])

  }

  implicit class RichDiscreteTerm[T](term: Term[GenericDiscreteDom[T]]) {
    def ===(that: Term[GenericDiscreteDom[T]]) = new DiscreteEquals(term, that)
  }

  implicit class RichToLog[T <: Term[Dom]](val term: T) {
    def logged(name: String) =
      term.domain.own(new LogTerm[Dom, Term[Dom]](term, name).asInstanceOf[TypedTerm[term.domain.Value]])

    def logged(implicit name: NameProvider) =
      term.domain.own(new LogTerm[Dom, Term[Dom]](term, name.newName()).asInstanceOf[TypedTerm[term.domain.Value]])

  }

  //  implicit class RichSeqTerm[E <: Dom, S <: Term[SeqDom[E]]](val term:S) {
  //    def apply[I <: Term[TypedDom[Int]]](index:I) =
  //      new SeqApply[E,S,I](term,index)
  //  }

  case class Assignment[+D <: Dom](variable: Var[D], value: D#Value) {
    def toSubstitution = Substitution[D](variable, variable.domain.Const(value.asInstanceOf[variable.domain.Value]))
  }

  case class Substitution[+D <: Dom](variable: Var[D], value: Term[D])

  implicit class RichVar[D <: Dom](val innerVar: Var[D]) {
    def <<(that: Term[D]) = Substitution(innerVar, that)

    def :=(that: Term[D]) = Substitution(innerVar, that)

    def <<(that: innerVar.domain.Value): Assignment[D] = Assignment(innerVar, that)

    def :=(that: innerVar.domain.Value): Assignment[D] = Assignment(innerVar, that)

  }

  implicit class RichTerm[A <: Term[Dom]](val innerTerm: A) {

    def |[D <: Dom](substitution: Substitution[D]): innerTerm.domain.Term = {
      innerTerm.domain.own(Substituted[Dom, Dom](innerTerm, substitution.variable, substitution.value).asInstanceOf[TypedTerm[innerTerm.domain.Value]])
    }

    def |[D <: Dom](assignment: Assignment[D]): innerTerm.domain.Term = this | assignment.toSubstitution

    def idx = indexed(innerTerm)(new CanonicalIndexer) //new IndexOf[innerTerm.domain.type](innerTerm.asInstanceOf[innerTerm.domain.Term])

    def index(implicit indexer: Indexer) = indexed(innerTerm)

    def eval(at: Assignment[Dom]*): innerTerm.domain.Value = {
      val values = at.map(a => a.variable -> a.value).toMap
      val args = innerTerm.vars.map(values)
      innerTerm.evalUntyped(args: _*)
    }

    def evalResult(at: Assignment[Dom]*):EvalResult[innerTerm.domain.Value] = {
      val values = at.map(a => a.variable -> a.value).toMap
      val args = innerTerm.vars.map(values)
      innerTerm.evalResultUntyped(args: _*)
    }

    //      assignments.foldLeft[AnyTerm](innerTerm) {
    //      case (result, assignment) => Substituted[Dom, Dom](result, assignment.variable, assignment.value)
    //    }.eval().asInstanceOf[innerTerm.domain.Value]

    def diff[D <: Dom](wrt: Var[D])(at: Assignment[Dom]*): wrt.domain.Value = {
      val values = at.map(a => a.variable -> a.value).toMap
      val args = innerTerm.vars.map(values)
      innerTerm.gradient(wrt, args: _*)
    }

    def map[B](fun: innerTerm.domain.Value => B)(implicit targetDom: TypedDom[B]): targetDom.Term = {
      val result = new TermMap[A, targetDom.type] {
        val term: innerTerm.type = innerTerm
        val domain: targetDom.type = targetDom

        def f(arg: term.domain.Value) = fun(arg)

        def isStatic = false

        def copy(args: IndexedSeq[ArgumentType]) = {
          val arg = new RichTerm(args(0))
          arg map fun.asInstanceOf[arg.innerTerm.domain.Value => B]
        }
      }
      targetDom.own(result)
    }

    def flatMap[B: TypedDom](fun: innerTerm.domain.Value => Term[TypedDom[B]]) = {
      val targetDom = implicitly[TypedDom[B]]
      new TermFlatMap[A, TypedDom[B]] {
        val term: innerTerm.type = innerTerm
        val domain: TypedDom[B] = targetDom

        def isStatic = false

        def f(arg: term.domain.Value): Term[TypedDom[B]] = fun(arg)
      }
    }

  }


  implicit class RichDom[D <: Dom](val dom: D) {
    //def x[D2 <: Dom](that: D2): Tuple2Dom[D, D2] = new Tuple2Dom[D, D2](dom, that)

    //    def iterator = dom.iterator
  }

  implicit class RichVect(val vect: Vect) {
    def toIndexedString(implicit index: Index) = {
      val mapped = vect.activeElements.filter(_._2 != 0.0).map(pair => index.key(pair._1) -> pair._2).toSeq
      val sorted = mapped.sortBy(-_._2)
      sorted.map(pair => s"${pair._2}\t${pair._1}").mkString("\n")
    }
  }

  ////  implicit class RichVarSeqTerm[E <: Dom, T <: Term[VarSeqDom[E]]](val term: T) {
  ////    def apply(index: Int) =
  ////      new VarSeqApply[E, T, term.domain.lengthDom.Term](term, term.domain.lengthDom.const(index))
  ////
  ////    def length = new VarSeqLength[T](term)
  ////
  ////    //def apply(index:Int) = term.elements(index)
  ////
  ////    def sum(body: term.domain.elementDom.Var => DoubleTerm) = {
  ////      new FirstOrderSum[E,DoubleTerm,T](term,???,???)
  ////    }
  ////
  ////  }
  //  implicit class RichVarSeqTerm[T <: Term[VarSeqDom[Dom]]](val term:T) {
  //    type NewE = term.domain.elementDom.type
  //    type NewT = Term[VarSeqDom[term.domain.elementDom.type]]
  //    type ResultType = Term[term.domain.elementDom.type]
  //    type ResultTypeOld = VarSeqApply[NewE,NewT,TypedTerm[Int]]
  //
  //  def apply(index:TypedTerm[Int]):ResultType =
  //      new VarSeqApply[Dom,Term[VarSeqDom[Dom]],TypedTerm[Int]](term,index).asInstanceOf[ResultType]
  //  }


}

trait MathImplicits {

  import TermImplicits._

  def Vectors(dim: Int) = new VectorDom(dim)

  def TypedVectors[D <: Dom](argDom: D, indexer: Indexer = new DefaultIndexer()) =
    new TypedVectorDom[argDom.type](argDom, indexer)

  def GrowableVectors(initDim: Int = 1000) = new GrowableVectorDom(initDim)

  def Matrices(dim1: Int, dim2: Int) = new MatrixDom(dim1: Int, dim2: Int)

  def vector(values: Double*): Vect = new DenseTensor1(values.toArray)

  def matrix(values: Seq[Double]*) = {
    val tmp = new DenseTensor2(values.length, values.head.length)

    (0 until values.length).foreach(row => {
      (0 until values(row).length).foreach(col => {
        tmp(row, col) = values(row)(col)
      })
    })

    tmp
  }


  def sigm[T <: DoubleTerm](term: T) = new Sigmoid(term)

  def sqrt[T <: DoubleTerm](term: T) = new Sqrt(term)

  def rect[T <: DoubleTerm](term: T) = new Rectifier(term)

  def tanh[T <: DoubleTerm](term: T) = new Tanh(term)

  def log[T <: DoubleTerm](term: T) = new Log(term)

  //  def I[T <: BoolTerm](term: T) = new Iverson(term)
  def I(term: BoolTerm) = new Iverson(term)

  def constraint(term: BoolTerm) = log(I(term))

  def sigmVec[T <: VectorTerm](term: T) = new VectorSigmoid(term)

  def tanhVec[T <: VectorTerm](term: T) = new VectorTanh(term)

  def min(arg1: DoubleTerm, arg2: DoubleTerm) = new Min2[DoubleTerm](arg1, arg2)

  def max(arg1: DoubleTerm, arg2: DoubleTerm) = new Max2[DoubleTerm](arg1, arg2)

  def l1[T <: VectorTerm](term: T) = new L1Norm[term.type](term)

  def l2[T <: VectorTerm](term: T) = sqrt(term dot term)

  def l2Squared[T <: VectorTerm](term: T) = term dot term

  implicit class RichVectTerm(val vect: VectorTerm) {

    def dot(that: VectorTerm) = new DotProduct(vect, that)

    def *(that: Term[DoubleDom]) = new VectorScaling(vect, that)

    def +(that: VectorTerm) = new VectorSum(IndexedSeq(vect, that))

    def -(that: VectorTerm) = new VectorSum(IndexedSeq(vect, that * (-1.0)))

    def conjoin(that: VectorTerm)(implicit index: DefaultIndexer, dom: VectorDom) = new Conjoined(vect, that)(index.index, dom)

    def apply(index: IntTerm) = new VectorApply(vect, index)

    //element-wise addition
    def :+(that: VectorTerm): VectorTerm = ???

    //element-wise multiplication
    def :*(that: VectorTerm): VectorTerm = ???

    def cons(that: VectorTerm): VectorTerm = new VectorConcatenation(vect, that)

    def l2(mask: VectorTerm = null): DoubleTerm = new SparseL2(vect, mask)
  }

  class RichTypedVectTerm[T <: Term[TypedVectorDom[Dom]]](val vect: T) {
    def apply(arg: vect.domain.argDom.Term) = new VectorApply(vect, indexed(arg)(vect.domain.indexer))
  }

  implicit def toRichTypedVectTerm(vect: Term[TypedVectorDom[Dom]]): RichTypedVectTerm[vect.type] =
    new RichTypedVectTerm[vect.type](vect)

  implicit class RichMatrixTerm(val mat: Term[MatrixDom]) {
    def dot(that: Term[MatrixDom]) = new MatrixDotProduct(mat, that)

    def *(that: VectorTerm) = new MatrixVectorProduct(mat, that)
  }

  implicit class RichDoubleTerm(term: DoubleTerm) {
    def +(that: DoubleTerm) = new Sum(IndexedSeq(term, that))

    def -(that: DoubleTerm) = new Sum(IndexedSeq(term, that * (-1.0)))

    //def -(that: IntTerm) = new Sum(IndexedSeq(term, that.toDouble * (-1.0)))

    //def *(that: DoubleTerm): Product = new Product(IndexedSeq(term, that))

    def *(that: DoubleTerm) = new Product(IndexedSeq(term, that))

    //    def *(that: IntTerm): Product = new Product(IndexedSeq(term, new IntToDouble(that)))

    def *(that: VectorTerm) = new VectorScaling(that, term)

    def /(that: DoubleTerm) = new Div(term, that)

    def >(that:DoubleTerm) = new GT(term,that)

    def unary_- = term * (-1.0)

    def subjectTo(predicate: BoolTerm) = term + constraint(predicate)

    def argmaxBy(factory: ArgmaxerFactory): ProxyTerm[TypedDom[Double]] = new ProxyTerm[TypedDom[Double]] {
      def self = term

      override def argmaxerImpl(wrt: Seq[Var[Dom]])(observed: Settings, msgs: Msgs) = {
        factory.argmaxer(term, wrt)(observed, msgs)
      }

      def copy(args: IndexedSeq[ArgumentType]) =
        new RichDoubleTerm(args(0)).argmaxBy(factory)

    }


  }

  implicit class StringEquals(val t: Any) {
    def stringEquals(that: Any) = t.toString == that.toString
  }

  implicit def intToRichIntTerm(int: Int): RichIntTerm = new RichIntTerm(int)

  implicit class RichIntTerm(val term: IntTerm) {
    def +(that: IntTerm) = new BinaryIntFun(term, that, "+", _ + _, _ + _)

    def -(that: IntTerm) = new BinaryIntFun(term, that, "-", _ - _, _ - _)

    def *(that: IntTerm) = new BinaryIntFun(term, that, "*", _ * _, (d1, d2) => Dom.ints)

    def until(end: IntTerm) = new RangeTerm(term, end)

    def toDouble = intToDouble(term)

  }

  def argmax[D <: Dom](dom: D)(obj: dom.Var => DoubleTerm): Argmax[dom.type] = {
    val variable = dom.Variable("_hidden")
    val term = obj(variable)
    new Argmax[dom.type](term, variable)
  }

  def sample[D <: Dom](dom: D)(obj: dom.Var => DoubleTerm)(implicit random: Random): Sample[dom.type] = {
    val variable = dom.Variable("_hidden")
    val term = obj(variable)
    new Sample[dom.type](term, variable)
  }


  abstract class LearnBuilder[D <: Dom](val dom: D) {
    def obj: dom.Var => DoubleTerm

    def using(argmaxerFactory: ArgmaxerFactory): dom.Value = (argmax(dom)(obj) by argmaxerFactory).eval()
  }

  def learn(dom: Dom)(objective: dom.Var => DoubleTerm) = new LearnBuilder[dom.type](dom) {
    def obj = objective
  }

  //def eval(term: AnyTerm)(assignments: Assignment[Dom]*): term.domain.Value = term.eval(assignments:_*)

  def max[D <: Dom](dom: D)(obj: dom.Var => DoubleTerm) = {
    val variable = dom.Variable("_hidden")
    val term = obj(variable)
    new Max(term, Seq(variable))
  }

  def sum[T](dom: Seq[T])(arg: T => DoubleTerm) = new Sum(dom.toIndexedSeq.map(arg))

  def sum(args: DoubleTerm*) = new Sum(args.toIndexedSeq)

  //def varSeqSum[T <: Term[VarSeqDom[TypedDom[Double]]]](args: T) = new VarSeqSum[TypedDom[Double], T](args)

  import TermImplicits._

  def sum(length: IntTerm)(args: DoubleTerm*): DoubleTerm =
    sum(args.toSeq, length)

  //varSeqSum[Term[VarSeqDom[TypedDom[Double]]]](VarSeq[TypedDom[Double]](length,args.toIndexedSeq.asInstanceOf[IndexedSeq[TypedTerm[Double]]]))

  def sum(args: Seq[DoubleTerm], length: IntTerm): DoubleTerm =
    new VarSeqSum(length, args)

  //varSeqSum[Term[VarSeqDom[TypedDom[Double]]]](VarSeq[TypedDom[Double]](length,args.toIndexedSeq.asInstanceOf[IndexedSeq[TypedTerm[Double]]]))


  def sum2[E <: Dom, T <: Term[VarSeqDom[E]], Body <: DoubleTerm](indices: T)(body: indices.domain.elementDom.Var => Body) = {
    val variable = indices.domain.elementDom.Variable("_i")
    val instantiatedBody = body(variable)
    new FirstOrderSum[E, Body, T](indices, variable, instantiatedBody)
  }

  def sum[T <: Term[VarSeqDom[Dom]], Body <: DoubleTerm](indices: T)(body: indices.domain.elementDom.Var => Body) =
    sum2[Dom, Term[VarSeqDom[Dom]], Body](indices)(body)

  def shuffled(indices: VarSeqDom[Dom]#Term)(body: indices.domain.elementDom.Term => DoubleTerm)(implicit random: Random) = {
    import TermImplicits._
    val i = mem(indices.sampleShuffled)
    val term = body(i.asInstanceOf[indices.domain.elementDom.Term])
    term
  }

  //  def oneHot(index: Int, value: Double = 1.0)(implicit dom: VectorDom) =
  //    dom.Const(new SingletonTensor1(dom.dim, index, value))

  def oneHot(index: IntTerm, value: DoubleTerm = Dom.doubles.Const(1.0))(implicit dom: VectorDom): VectorTerm =
    OneHot(index, value)

  def feature(feat: AnyTerm, value: DoubleTerm = Dom.doubles.Const(1.0))(implicit dom: VectorDom, indexer: Indexer) =
    oneHot(indexed(feat), value)

  def feature(name: Symbol, keys: AnyTerm*)(implicit dom: VectorDom, index: Index) =
    Feature(name, keys.toIndexedSeq, Doubles.one)(index, dom)

  def feature(name: Symbol, value: DoubleTerm, keys: AnyTerm*)(implicit dom: VectorDom, index: Index) =
    Feature(name, keys.toIndexedSeq, value)(index, dom)

  implicit def doubleToConstant(d: Double): Dom.doubles.Term = Dom.doubles.Const(d)

  implicit def intToConstant(i: Int): Constant[IntDom] = new RangeDom(i until i + 1).Const(i)

  implicit def doubleToRichConstant(d: Double): RichDoubleTerm = new RichDoubleTerm(doubleToConstant(d))

  //implicit def anyToConst[T](any: T)(implicit dom: TypedDom[T]): dom.Term = dom.Const(any)

  implicit def intToDoubleConstant(d: Int): Dom.doubles.Term = Dom.doubles.Const(d)

  implicit def vectToConstant(d: Vect): Constant[VectorDom] = Vectors(d.dim1).Const(d)

  implicit def vectToConstantWithDom(d: Vect)(implicit dom: VectorDom): dom.Term = dom.Const(d)

  implicit def matToConstant(d: Mat): Constant[MatrixDom] = Matrices(d.dim1, d.dim2).Const(d)

  implicit def intToDouble(int: IntTerm): IntToDouble[int.type] = new IntToDouble[int.type](int)

  implicit def valueToConst[T <: AnyRef](value: T)(implicit dom: TypedDom[T]): dom.Term = dom.Const(value)


  def boolToInt(bool: BoolTerm): Dom.ints.Term = {
    Dom.ints.own(bool.asInstanceOf[IntTerm])
  }

}

trait Stochastic {

  this:MathImplicits =>

  import TermImplicits._

  def sampleSequential(range: Range) = Ints(range).sequential

  def sampleUniform(range: Range)(implicit random: Random) = Ints(range).uniform

  def nextBoolean()(implicit random: Random) = nextDouble() > 0.5

  def nextDouble()(implicit random: Random) = new SampleDouble(random.nextDouble())

}

trait LoggedTerms extends NamedValueImplicits {

  def logged[T <: Term[Dom]](term: NamedValue[T]): term.value.domain.Term = {
    val clean = term.name.replace("ml.wolfe.term.TermImplicits.", "")
    term.value.domain.own(new LogTerm[Dom, Term[Dom]](term.value, clean).asInstanceOf[TypedTerm[term.value.domain.Value]])
  }

}

trait FVectors {
  def feats(prefix: Symbol, elements: Seq[Any], value: Double = 1.0)(implicit index: Index) = {
    val result = new GrowableSparseTensor1(elements)
    for (e <- elements) {
      result(index(prefix -> e)) = value
    }
    result
  }

  def feats(elements: Any*)(implicit index: Index): Vect = {
    val result = new GrowableSparseTensor1(elements)
    for (e <- elements) {
      result(index(e)) = 1.0
    }
    result
  }


}


