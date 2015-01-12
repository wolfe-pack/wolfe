package ml.wolfe.fg

import ml.wolfe.FactorGraph.{Node, Edge}
import ml.wolfe.MoreArrayOps._
import ml.wolfe.{Wolfe, FactorieVector, FactorGraph}
// import scalaxy.loops._
import scala.util.Random
import cc.factorie.la.DenseTensor1
import math._

/**
 * @author Sebastian Riedel
 */
trait Var[T] {

  import Wolfe.notSupported

  def asTyped[Q] = this.asInstanceOf[Var[Q]]
  def asDiscrete = this.asInstanceOf[DiscreteVar[_]]
  def asDiscreteTyped[Q] = this.asInstanceOf[DiscreteVar[Q]]
  def asVector = this.asInstanceOf[VectorVar]
  def asTuple = this.asInstanceOf[TupleVar]
  def asContinuous = this.asInstanceOf[ContinuousVar]

  def setup(){}
  def createMsgs():Msgs = ???
  protected var observed: Option[T] = None
  def isObserved = observed != None
  def observe(value:T) = observed = Some(value)

  def initializeToNegInfinity():Unit = notSupported
  def initializeRandomly(eps:Double):Unit = notSupported
  def updateN2F(edge: Edge):Unit = notSupported
  def powN2F():Unit = notSupported
  def updateDualN2F(edge: Edge, stepSize:Double):Unit = notSupported
  def fixMapSetting(overwrite:Boolean = false):Unit = notSupported
  def setToArgmax():Unit = notSupported
  def deterministicN2F(edge: Edge):Unit = notSupported
  def updateMaxMarginalBelief():Unit = notSupported
  def updateMarginalBelief():Unit = notSupported
  def updateAverageBelief():Unit = notSupported
  def entropy():Double = notSupported

  type S
  var setting: S
  def value:T


  /* A description of what the variable represents */
  val label:String = ""
  
  var node:Node = null //updated in node init
}

class DiscreteVar[T](var domain: Array[T], override val label:String = "") extends Var[T] {

  def dim = domain.length

  /* node belief */
  var b = Array.ofDim[Double](dim)

  /* external message for this node. Will usually not be updated during inference */
  var in = Array.ofDim[Double](dim)

  type S = Int
  /* indicates that variable is in a certain state */
  override var setting: Int = 0

  /* indicates the value corresponding to the setting of the node */
  override def value:T = observed match {
    case Some(value) => value
    case None => domain(setting)
  }
  override def isObserved = dim == 1 || super.isObserved
  override def createMsgs() = new DiscreteMsgs(dim)

  override def entropy() = {
    var result = 0.0
    for (i <- (0 until b.length)) {
      result -= math.exp(b(i)) * b(i) //messages are in log space
    }
    result
  }

  override def initializeToNegInfinity() = {
    fill(b, Double.NegativeInfinity)
  }


  override def initializeRandomly(eps:Double) = {
    for (i <- b.indices) b(i) = Random.nextGaussian() * eps
  }

  override def setup() {
    if (b == null || b.length != dim) b = Array.ofDim[Double](dim)
    if (in == null || in.length != dim) in = Array.ofDim[Double](dim)
  }

  override def updateN2F(edge: Edge) = {
    val node = edge.n
    val m = edge.msgs.asDiscrete
    System.arraycopy(in, 0, m.n2f, 0, m.n2f.length)
    for (i <- (0 until dim)) {
      for (e <- (0 until node.edges.length); if e != edge.indexInNode)
        m.n2f(i) += node.edges(e).msgs.asDiscrete.f2n(i)
    }
  }

  override def powN2F() = {
    val sums = in.clone()
      for (i <- (0 until dim))
        for (e <- (0 until node.edges.length))
          sums(i) += node.edges(e).msgs.asDiscrete.f2n(i)

      for (i <- (0 until dim))
        for (e <- (0 until node.edges.length))
          node.edges(e).msgs.asDiscrete.n2f(i) = sums(i) - node.edges(e).msgs.asDiscrete.n2f(i)
  }

  override def updateDualN2F(edge:Edge, stepSize:Double) = {
    val m = edge.msgs.asDiscrete
    for(i <- (0 until m.n2f.size))
      m.n2f(i) = m.n2f(i) - stepSize * (m.f2n(i) - b(i)) }

  var fixedSetting = false
  override def fixMapSetting(overwrite:Boolean = false):Unit = {
    if(! fixedSetting || overwrite) {
      var maxScore = Double.NegativeInfinity
      for (i <- (0 until dim)) {
        var score = in(i)
        for (e <- (0 until node.edges.length))
          score += node.edges(e).msgs.asDiscrete.f2n(i)
        if (score > maxScore) {
          maxScore = score
          setting = i
        }
        fixedSetting = true
      }
    }
  }

  override def setToArgmax():Unit = {
    if(! fixedSetting)
      setting = ml.wolfe.MoreArrayOps.maxIndex(b)
  }

  override def deterministicN2F(edge: Edge) = {
    val m = edge.msgs.asDiscrete
    fill(m.n2f, Double.NegativeInfinity)
    m.n2f(setting) = 0
  }

  override def updateMaxMarginalBelief() = {
    System.arraycopy(in, 0, b, 0, b.length)
    for (e <- 0 until node.edges.length) {
      for (i <- 0 until dim)
        b(i) += node.edges(e).msgs.asDiscrete.f2n(i)
      maxNormalize(b)
    }
  }
  override def updateMarginalBelief() = {
    System.arraycopy(in, 0, b, 0, b.length)
    for (e <- 0 until node.edges.length) {
      for (i <- 0 until dim)
        b(i) += node.edges(e).msgs.asDiscrete.f2n(i)
      val logZ = math.log(b.view.map(exp).sum)
      -=(b,logZ)
    }
  }

  override def updateAverageBelief() = {
    System.arraycopy(in, 0, b, 0, b.length)
    for (e <- 0 until node.edges.length)
      for (i <- 0 until dim)
        b(i) += node.edges(e).msgs.asDiscrete.f2n(i)
    for(i <- 0 until dim) b(i) = b(i) / node.edges.length
  }

}

class ContinuousVar(override val label:String = "") extends Var[Double] {
  type S = Double
  /* indicates that variable is in a certain state */
  override var setting: Double = 0
  override def value = observed match {
    case Some(value) => value
    case None => setting
  }

  override def createMsgs() = null
}

class VectorVar(val dim:Int, override val label:String) extends Var[FactorieVector] {
  var b:FactorieVector = new DenseTensor1(dim)
  var unitVector = false

  override type S = FactorieVector
  override var setting:FactorieVector = null
  override def value = setting

  override def createMsgs() = null

  override def updateN2F(edge: Edge) = {
    edge.msgs.asVector.n2f = b //todo: copy instead?
  }

  override def initializeRandomly(eps:Double) = {
    for (i <- 0 until dim) b(i) = Random.nextGaussian() * eps
  }
  override def initializeToNegInfinity() = {
    b := Double.NegativeInfinity
  }
}


