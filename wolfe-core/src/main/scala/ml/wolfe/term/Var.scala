package ml.wolfe.term

import scala.collection.mutable

/**
 * @author riedel
 */
trait Var[+D <: Dom] extends Term[D] {
  self =>
  def name: String

  def ranges: Ranges

  def owner: Var[Dom]

  def ownerOrSelf: Var[Dom] = if (owner == null) self else owner

  def vars = if (owner == null) Seq(this) else Seq(owner)

  def isStatic = false

  def differentiatorOld(wrt: Seq[Var[Dom]]) = new DifferentiatorOld {
    def term = self

    def withRespectTo = wrt

    def forwardProp(current: Array[Setting]) = {
      ranges.copy(current(0), activation)
    }

    def backProp(error: Setting, gradient: Array[Setting]) = {
      ranges.addIntoIfChanged(error, gradient(0))
    }
  }

  def evaluatorOld() = new EvaluatorOld {

    def eval(inputs: Array[Setting], output: Setting) = {
      ranges.copy(inputs(0), output)
    }
  }


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

trait Atom[D <: Dom] extends Var[D] {

  val name = toString
  def atomsIterator = ???
  def owner = ???
  def ranges = ???
}

case class VarAtom[D<:Dom](variable:Var[D]) extends Atom[D] {
  val domain = variable.domain
}

case class _1Atom[D <: Dom](parent:Atom[Tuple2Dom[D,_]]) extends Atom[D] {
  val domain = parent.domain.dom1
}
case class _2Atom[D <: Dom](parent:Atom[Tuple2Dom[_,D]]) extends Atom[D] {
  val domain = parent.domain.dom2
}

case class SeqAtom[E <: Dom, S <: VarSeqDom[E]](seq:Atom[S],index:IntTerm) extends Var[E] {

  val name = toString

  val domain = seq.domain.elementDom

  def atomsIterator = ???
  def owner = ???
  def ranges = ???

}


trait AtomOld[+D <: Dom] extends Var[D] {
  def offset: Int

  def atomsIterator = Iterator(this)

  override def hashCode() = offset

  override def equals(obj: scala.Any) = obj match {
    case a: AtomOld[_] =>
      a.domain == domain &&
        (a.ownerOrSelf eq ownerOrSelf) &&
        a.offset == offset
    case _ => false
  }

  def projectValue(setting: Setting) {}

}

case class AtomsOld(disc: Seq[DiscVar[Any]] = Nil, cont: Seq[DoubleVar] = Nil, vect: Seq[VectorVar] = Nil, mats: Seq[MatrixVar] = Nil) {
  def ++(that: AtomsOld) = copy(disc = disc ++ that.disc, cont = cont ++ that.cont, vect = vect ++ that.vect, mats = mats ++ that.mats)

  def merge(that: AtomsOld) = copy(
    disc = (disc ++ that.disc).distinct,
    cont = (cont ++ that.cont).distinct,
    vect = (vect ++ that.vect).distinct,
    mats = (mats ++ that.mats).distinct)

  def filterByOwner(predicate: Var[Dom] => Boolean) = copy(
    disc = disc.filter(v => predicate(v.owner)),
    cont = cont.filter(v => predicate(v.owner)),
    vect = vect.filter(v => predicate(v.owner)),
    mats = mats.filter(v => predicate(v.owner))
  )

  def distinct = copy(
    disc = disc.distinct,
    cont = cont.distinct,
    vect = vect.distinct,
    mats = mats.distinct
  )

}

case class AtomIndicesOld(disc: Array[Int], cont: Array[Int], vect: Array[Int], mats: Array[Int])

object AtomsOld {
  def fromIterator(iterator: Iterator[AtomOld[Dom]]) = {
    val cont = new mutable.HashSet[DoubleVar]()
    val vect = new mutable.HashSet[VectorVar]()
    val disc = new mutable.HashSet[DiscVar[Any]]()
    val mats = new mutable.HashSet[MatrixVar]()

    for (a <- iterator) a match {
      case c: DoubleVar => cont += c
      case v: VectorVar => vect += v
      case m: MatrixVar => mats += m
      case d: DiscVar[_] => disc += d.asInstanceOf[DiscVar[Any]]
    }
    AtomsOld(disc.toSeq, cont.toSeq, vect.toSeq, mats.toSeq)
  }
}