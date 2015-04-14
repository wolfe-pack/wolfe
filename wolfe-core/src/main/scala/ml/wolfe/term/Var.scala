package ml.wolfe.term

/**
 * @author riedel
 */
trait Var[+D <: Dom] extends Term[D] {
  self =>
  def varName: String

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

  override def toString = varName
}

/**
 * An atom is a variable that can be used to represent a part of another, possibly structured, owner variable.
 * For example, we can use an atom x(i) to represent the i-ths element of a sequence variable x.
 * @tparam D the domain of the variable.
 */
trait Atom[+D <: Dom] extends Var[D] {

  self =>

  trait Grounder {
    def ground()(implicit execution: Execution): GroundAtom[D]
  }

  def varsToGround: Seq[AnyVar]

  def grounder(settings: Settings): Grounder

  def owner: AnyVar

}

/**
 * A ground atom is an atom that has no free variables in its definition. For example, x(i) is a non-ground atom
 * as the index i to the sequence variable x is a free variable. The atom x(4) on the other hand is grounded.
 * @tparam D the domain of the variable.
 */
trait GroundAtom[+D <: Dom] extends Atom[D] {
  self =>
  def offsets: Offsets

  def varsToGround:Seq[AnyVar] = Nil

  def grounder(settings: Settings) = new Grounder {
    def ground()(implicit execution: Execution) = self
  }
}

case class VarAtom[D <: Dom](variable: Var[D]) extends GroundAtom[D] {

  def offsets = Offsets.zero

  val domain = variable.domain

  def owner = variable

  def varName = variable.varName
}

//case class _1Atom[D <: Dom](parent:Atom[Tuple2Dom[D,_]]) extends Atom[D] {
//  val domain = parent.domain.dom1
//
//}
//case class _2Atom[D <: Dom](parent:Atom[Tuple2Dom[_,D]]) extends Atom[D] {
//  val domain = parent.domain.dom2
//
//}

case class SeqGroundAtom[E <: Dom, S <: VarSeqDom[E]](seq: GroundAtom[S], index: Int) extends GroundAtom[E] {
  val domain = seq.domain.elementDom

  val offsets = seq.offsets + Offsets(1, 0, 0, 0, 0) + domain.lengths * index

  def owner = seq.owner

  def varName = getClass.getSimpleName + "(" +
    (if(productArity==0) "" else productIterator.map(_.toString).reduce(_ + "," + _)) + ")"
}

case class SeqAtom[E <: Dom, S <: VarSeqDom[E]](seq: Atom[S], index: IntTerm) extends Atom[E] {

  val domain = seq.domain.elementDom
  val varsToGround = (seq.varsToGround ++ index.vars).distinct

  def grounder(settings: Settings) = {
    new Grounder {
      val seqGrounder = seq.grounder(settings.linkedSettings(varsToGround, seq.varsToGround))
      val indexEval = index.evaluatorImpl(settings.linkedSettings(varsToGround, index.vars))

      def ground()(implicit execution: Execution) = {
        val parent = seqGrounder.ground()
        indexEval.eval()
        val groundIndex = indexEval.output.disc(0)
        SeqGroundAtom[E,S](parent,groundIndex)
      }
    }
  }
  def varName = getClass.getSimpleName + "(" +
    (if(productArity==0) "" else productIterator.map(_.toString).reduce(_ + "," + _)) + ")"
  def owner = seq.owner
}

trait GenericLengthAtom[S<:VarSeqDom[_]] extends Atom[IntDom]{
  def seq:Atom[S]
  val domain = seq.domain.lengthDom
  def owner = seq.owner
  //def name = toString
}

case class LengthAtom[S<:VarSeqDom[_]](seq:Atom[S]) extends GenericLengthAtom[S] {
  val varsToGround = seq.varsToGround
  def grounder(settings: Settings) = new Grounder {
    val seqGrounder = seq.grounder(settings.linkedSettings(varsToGround, seq.varsToGround))
    def ground()(implicit execution: Execution) = {
      val parent = seqGrounder.ground()
      LengthGroundAtom[S](parent)
    }
  }
  def varName = getClass.getSimpleName + "(" +
    (if(productArity==0) "" else productIterator.map(_.toString).reduce(_ + "," + _)) + ")"
}

case class LengthGroundAtom[S <: VarSeqDom[_]](seq:GroundAtom[S]) extends GroundAtom[IntDom] with GenericLengthAtom[S] {
  def offsets = seq.offsets
  def varName = getClass.getSimpleName + "(" +
    (if(productArity==0) "" else productIterator.map(_.toString).reduce(_ + "," + _)) + ")"
}





