package ml.wolfe.term

import ml.wolfe.term

/**
 * @author riedel
 */
class Tuple2Dom[D1 <: Dom, D2 <: Dom](val dom1: D1, val dom2: D2) extends TupleDom {
  dom =>
  type Value = (dom1.Value, dom2.Value)
  type Var = Tuple2Var
  type Term = Tuple2Term
  type Marginals = (dom1.Marginals, dom2.Marginals)

  def productArity = 2

  def fieldName(index: Int) = "_" + index

  val domains = IndexedSeq(dom1,dom2)
  val offsets = IndexedSeq(Offsets.zero, dom1.lengths)

  def fieldDomain(index: Int) = domains(index)
  def fieldOffset(index: Int) = offsets(index)

  trait Tuple2Term extends TupleTerm {
    def _1:dom1.Term = dom1.own(productElement(0).asInstanceOf[TypedTerm[dom1.Value]])
    def _2:dom2.Term = dom2.own(productElement(1).asInstanceOf[TypedTerm[dom2.Value]])
  }

  class Tuple2Var(val name:String) extends DomVar with TupleView with Tuple2Term {
    def product = this
  }

  class Tuple2Constructor(a1:dom1.Term,a2:dom2.Term) extends Tuple2Term with TupleConstructor {
    def productElements = IndexedSeq(a1,a2)
    def copy(args: IndexedSeq[ArgumentType]) =
      new Tuple2Constructor(args(0).asInstanceOf[dom1.Term],args(1).asInstanceOf[dom2.Term])
  }

  val lengths = dom1.lengths + dom2.lengths

  override val dimensions = dom1.dimensions + dom2.dimensions

  def toValue(setting: Setting, offsets: Offsets = Offsets()) = {
    val arg1 = dom1.toValue(setting, offsets)
    val arg2 = dom2.toValue(setting, offsets + dom1.lengths)
    (arg1, arg2)
  }


  //trait Test extends Term
  def own(term: TypedTerm[Value]) = new OwnedTerm[Value] with TupleView with Tuple2Term {
    def product = term
    def self = term
    def copy(args: IndexedSeq[ArgumentType]) = own(args(0))
    override val domain: dom.type = dom

  }

  def toMarginals(msg: Msg, offsets: Offsets) = {
    val arg1 = dom1.toMarginals(msg, offsets)
    val arg2 = dom2.toMarginals(msg, offsets + dom1.lengths)
    (arg1, arg2)
  }

  def copyValue(value: Value, setting: Setting, offsets: Offsets = Offsets()): Unit = {
    dom1.copyValue(value._1, setting, offsets)
    dom2.copyValue(value._2, setting, offsets + dom1.lengths)
  }

  def copyMarginals(marginals: Marginals, msgs: Msg, offsets: Offsets = Offsets()): Unit = {
    dom1.copyMarginals(marginals._1, msgs, offsets)
    dom2.copyMarginals(marginals._2, msgs, offsets + dom1.lengths)
  }

  def fillZeroMsg(target: Msg, offsets: Offsets) = {
    dom1.fillZeroMsg(target, offsets)
    dom2.fillZeroMsg(target, offsets + dom1.lengths)
  }


  def one = (dom1.one, dom2.one)

  def zero = (dom1.zero, dom2.zero)

  def Const(value: Value) = new Tuple2Constructor(dom1.Const(value._1),dom2.Const(value._2))
  def variable(name: String) = new Tuple2Var(name)

  object Term {
    def unapply(term:AnyTerm) = term match {
      case t:Tuple2Term => Some(t._1,t._2)
      case _ => None
    }
  }

}

trait TupleDom extends Dom {
  dom =>
  type Value <: scala.Product

  def productName = dom.getClass.getSimpleName
  def productArity:Int
  def fieldName(index:Int):String
  def fieldOffset(index:Int):Offsets
  def fieldDomain(index:Int):Dom

  trait TupleTerm extends DomTerm {
    def productElement(index:Int):AnyTerm
  }

  trait TupleView extends TupleTerm {
    def product:TypedTerm[Value]
    def productElement(index: Int) =
      new Field[Dom,Value](product,fieldDomain(index),fieldOffset(index))(fieldName(index))

  }

  trait TupleConstructor extends TupleTerm with Composed[Dom] {
    def productElements:IndexedSeq[AnyTerm]
    def productElement(index: Int) = productElements(index)
    type ArgumentType = AnyTerm
    val arguments = productElements

    override def composer(args: Settings) = new Composer(args) {
      def eval()(implicit execution: Execution) = {
        for (i <- 0 until productArity) {
          input(i).copyTo(output, fieldOffset(i), 1)
        }
      }
    }
  }

}

trait ProductDom extends Dom {
  dom =>

  type Value <: scala.Product

  def productName = dom.getClass.getSimpleName



  trait DomTermImpl extends super.DomTerm with Composed[dom.type] {


    override def toString = s"""$productName(${arguments.mkString(",")})"""

    override def composer(args: Settings) = new Composer(args) {
      val lengths = arguments.map(_.domain.lengths).toArray

      def eval()(implicit execution: Execution) = {
        var off = Offsets()
        for (i <- 0 until input.length) {
          input(i).copyTo(output, off, 1)
          off += lengths(i)
        }
        //append inputs to output

      }
    }

  }

}

class Field[D <: Dom, Value](val product: TypedTerm[Value], val domain: D, val start: Offsets)(fieldName:String = start.toString) extends Composed[D] {

  type ArgumentType = TypedTerm[Value]
  val arguments = IndexedSeq(product)

  def copy(args: IndexedSeq[ArgumentType]) = new Field(args(0),domain,start)(fieldName)

  override def composer(args: Settings) = new Composer(args) {

    //input(0).recordChangedOffsets = true

    def eval()(implicit execution: Execution) = {
      input(0).copyTo(output,start,Offsets.zero,domain.lengths)
    }
  }

  override def differentiatorImpl(wrt: Seq[term.Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) =
    new ComposedDifferentiator(wrt,in,err,gradientAcc) {
      def localBackProp()(implicit execution: Execution) = {
        //todo: SR: why can't argErrors be reset to zero in the ComposedDiff code?
        argErrors(0).resetToZero()
        error.copyTo(argErrors(0),Offsets.zero,start,domain.lengths)
      }
    }

  def composerOld() = ???

  def differentiatorOld(wrt: Seq[term.Var[Dom]]) = ???

  override def toString = s"$product.$fieldName"
}
