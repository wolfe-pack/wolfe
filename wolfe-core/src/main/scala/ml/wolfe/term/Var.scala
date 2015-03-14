package ml.wolfe.term

import scala.collection.mutable

/**
 * @author riedel
 */
trait Var[+D <: Dom] extends Term[D] {
  self =>
  def name: String

  def vars = Seq(this)

  def isStatic = false

  override def evaluatorImpl(in: Settings) = new Evaluator {
    def eval()(implicit execution: Execution) = {}

    val input = in
    val output = in(0)
  }


  override def differentiatorImpl(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) =
    new AbstractDifferentiator(in, err, gradientAcc, wrt) {
      val output = in(0)
      val isWrt = wrt.contains(self)

      def forward()(implicit execution: Execution) {}

      def backward()(implicit execution: Execution): Unit = {
        if (isWrt) gradientAccumulator(0).addIfChanged(error)
      }
    }

  override def toString = name
}

trait Atom[+D <: Dom] extends Var[D] {

  val name = toString

  def id()(implicit execution: Execution):Any

}

case class VarAtom[D<:Dom](variable:Var[D]) extends Atom[D] {
  val domain = variable.domain

  def id()(implicit execution: Execution) = variable
}

case class _1Atom[D <: Dom](parent:Atom[Tuple2Dom[D,_]]) extends Atom[D] {
  val domain = parent.domain.dom1
  def id()(implicit execution: Execution) = (parent.id(),1)

}
case class _2Atom[D <: Dom](parent:Atom[Tuple2Dom[_,D]]) extends Atom[D] {
  val domain = parent.domain.dom2
  def id()(implicit execution: Execution) = (parent.id(),2)

}

case class SeqAtom[E <: Dom, S <: VarSeqDom[E]](seq:Atom[S],index:IntTerm) extends Atom[E] {

  val domain = seq.domain.elementDom
  def id()(implicit execution: Execution) = (seq.id(),2)


}








