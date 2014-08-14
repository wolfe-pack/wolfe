package ml.wolfe.fg

import ml.wolfe.FactorGraph._
import ml.wolfe.FactorieVector
import ml.wolfe.util.Multidimensional._

/**
 * @author Sebastian Riedel
 */
trait Potential {

  def notSupported = sys.error("This function is not supported")

  /**
   * Calculate and update the max-marginal message from the factor of this potential to
   * the given edge.
   * @param edge the edge to pass the message on.
   */
  def maxMarginalF2N(edge: Edge):Unit = notSupported

  /**
   * Calculate and update the marginal message from the factor of this potential to
   * the given edge.
   * @param edge the edge to pass the message on.
   */
  def marginalF2N(edge: Edge):Unit = notSupported

  /**
   * Calculate and update the MAP messages from the factor of this potential to all edges.
   */
  def mapF2N():Unit = notSupported

  /**
   * Calculate and update the quadratic program messages from the factor of this
   * potential to all edges, as used in AD3.
   */
  def quadraticProgramF2N(stepSize:Double, maxIterations:Int):Unit = notSupported

  def maxMarginalExpectationsAndObjective(dstExpectations: FactorieVector): Double = notSupported
  def marginalExpectationsAndObjective(dstExpectations: FactorieVector): Double = notSupported
  def valueForCurrentSetting(): Double = notSupported
  def valueAndGradientForAllEdges():Double = notSupported
  def isLinear = false
  def statsForCurrentSetting(): FactorieVector = null
  def toVerboseString(implicit fgPrinter: FGPrinter): String = getClass.getName
  def toHTMLString(implicit fgPrinter: FGPrinter): String = toVerboseString(fgPrinter)
  var factor:Factor = null //Automatically set by Factor

}

trait DiscretePotential extends Potential {
  lazy val vars:Array[DiscreteVar] = factor.edges.map(_.n.variable.asDiscrete)
  def valueForSetting(setting:Seq[Int]): Double
  override def valueForCurrentSetting(): Double = valueForSetting(vars.view.map(_.setting))

  /**
   * The table of scores assigned by the potential over different settings, as a LabelledTensor
   * @return a LabelledTensor containing the scores
   */
  protected def getScoreTable : LabelledTensor[DiscreteVar, Double] = {
    val t:LabelledTensor[DiscreteVar, Double] = LabelledTensor.onNewArray(vars, _.dim, 0.0)
    t.fillBy(mul => valueForSetting(mul.map(_._2)))
    t
  }
  protected lazy val $scoreTable = getScoreTable
  def scoreTable = if(isLinear) getScoreTable else $scoreTable
}

final class AndPotential(arg1: Edge, arg2: Edge) extends DiscretePotential {
  val n1 = arg1.n.variable.asDiscrete
  val n2 = arg2.n.variable.asDiscrete
  val m1 = arg1.msgs.asDiscrete
  val m2 = arg2.msgs.asDiscrete

  override def maxMarginalF2N(edge: Edge) = {
    val msgs = edge.msgs.asDiscrete
    val other = if (edge == arg1) m2 else m1
    msgs.f2n(1) = other.n2f(1)
    msgs.f2n(0) = Double.NegativeInfinity
  }
  override def valueForSetting(setting:Seq[Int]) = {
    if (setting(0)==1 && setting(1)==1) 0.0 else Double.NegativeInfinity
  }
  override def maxMarginalExpectationsAndObjective(dstExpectations: FactorieVector) = {
    val beliefInConsistentSolution = m1.n2f(1) + m2.n2f(1)
    if (beliefInConsistentSolution > Double.NegativeInfinity) 0.0 else Double.NegativeInfinity
  }
  override def toVerboseString(implicit fgPrinter: FGPrinter) = s"And(${ arg1.n.index },${ arg2.n.index })"

}

final class ExactlyOncePotential(args: Seq[Edge]) extends Potential {
  val msgs = args.map(_.msgs.asDiscrete)
  override def toVerboseString(implicit fgPrinter: FGPrinter) = {
    val argIndices = args map (_.n.index) mkString ","
    s"ExactlyOne($argIndices)"
  }
  override def valueForCurrentSetting():Double = {
    var count = 0
    for (arg <- msgs) {
      if (arg.f2n(1) == 1) count += 1
      if (count > 1) return Double.NegativeInfinity
    }
    if (count == 1) 0.0 else Double.NegativeInfinity
  }

  override def maxMarginalExpectationsAndObjective(dstExpectations: FactorieVector):Double = {
    //objective usually zero unless all choices are impossible
    for (arg <- msgs) if (arg.n2f(1) > Double.NegativeInfinity) return 0.0
    Double.NegativeInfinity
  }

  override def maxMarginalF2N(edge: Edge) = {
    var othersFalseScore = 0.0
    var winningDelta = Double.NegativeInfinity
    for (e <- msgs; if e != edge.msgs) {
      othersFalseScore += e.n2f(0)
      val delta = e.n2f(1) - e.n2f(0)
      if (delta > winningDelta) {
        winningDelta = delta
      }
    }
    edge.msgs.asDiscrete.f2n(1) = othersFalseScore
    edge.msgs.asDiscrete.f2n(0) = othersFalseScore + winningDelta

  }
}





