package ml.wolfe.fg20

import cc.factorie.la.DenseTensor1
import ml.wolfe.{FactorieVector, SparseVector, FactorGraph}
import ml.wolfe.MoreArrayOps._


class BruteForce(val problem: Problem) extends NodeContentFG with EmptyFactorFG with EmptyEdgeFG {
  type NodeContent = Any
  type ContNodeContent = Nothing
  type VectNodeContent = Nothing
  class DiscNodeContent(var belief: Array[Double])
  def createDiscNodeContent(variable: DiscVar[Any]) = new DiscNodeContent(Array.ofDim[Double](variable.dom.size))
  def createContNodeContent(contVar: ContVar) = sys.error("Can't do brute force with continuous variables")
  def createVectNodeContent(vectVar: VectVar) = sys.error("Can't do brute force with vector variables")
  def acceptPotential = { case p: Potential => p }
  type Pot = Potential

  def loopOverSettings(nodes: List[DiscNode], loop: (() => Unit) => Unit = body => body()): (() => Unit) => Unit = {
    nodes match {
      case Nil => (body: () => Unit) => loop(body)
      case head :: tail =>
        def newLoop(body: () => Unit) {
          if (!head.observed) for (setting <- head.variable.dom.indices) {
            head.setting = setting
            loop(body)
          } else loop(body)
        }
        loopOverSettings(tail, newLoop)
    }
  }

  def currentScore(weights: FactorieVector) = {
    factors.view.map(f => f.pot.score(f, weights)).sum
  }

  def inferMAP(weights: FactorieVector = new DenseTensor1(0)): MAPResult = {
    val nodes = var2DiscNode.values.toList
    val loop = loopOverSettings(nodes)
    var maxScore = Double.NegativeInfinity
    for (n <- nodes) fill(n.content.belief, Double.NegativeInfinity)

    var maxSetting: Array[Int] = null
    loop { () =>
      val score = currentScore(weights)
      for (n <- nodes) {
        n.content.belief(n.setting) = math.max(score, n.content.belief(n.setting))
      }

      if (score > maxScore) {
        maxScore = score
        maxSetting = nodes.view.map(_.setting).toArray
      }
    }

    for ((s, n) <- maxSetting zip nodes; if !n.observed) {
      n.setting = s
      maxNormalize(n.content.belief)
    }

    val gradient = new SparseVector(1000)
    for (f <- factors; if f.pot.isLinear)
      gradient += f.pot.statsForCurrentSetting(f)

    val state = nodes.map(n => n.variable -> n.variable.dom(n.setting))
    val maxMarginals = nodes.map(n =>
      DiscBelief(n.variable) -> Distribution.disc(n.variable.dom, n.content.belief.map(math.exp)))
    MAPResult(new MapBasedState(state.toMap), maxScore, gradient, new MapBasedState(maxMarginals.toMap))
  }

  def inferMarginals(weights: FactorieVector = new DenseTensor1(0)): MarginalResult = {
    val nodes = var2DiscNode.values.toList
    val loop = loopOverSettings(nodes)
    var Z = 0.0
    for (n <- nodes) fill(n.content.belief, 0.0)

    loop { () =>
      val score = currentScore(weights)
      val prob = math.exp(score)
      for (n <- nodes) {
        n.content.belief(n.setting) += prob
      }
      Z += prob
    }

    for (n <- nodes) {
      normalize(n.content.belief)
      log(n.content.belief)
    }

    val logZ = math.log(Z)
    val gradient = new SparseVector(1000)

    loop { () =>
      val score = currentScore(weights)
      val prob = math.exp(score) / Z
      for (f <- factors; if f.pot.isLinear)
        gradient +=(f.pot.statsForCurrentSetting(f), prob)
    }

    val marginals = nodes.map(n =>
      DiscBelief(n.variable) -> Distribution.disc(n.variable.dom, n.content.belief.map(math.exp)))
    MarginalResult(logZ, gradient, new MapBasedState(marginals.toMap))
  }


}


class BruteForce2(val problem: Problem) extends NodeContentFG2 with EmptyEdgeFG2 {

  class FactorType(val pot: Pot) extends Factor {
    var setting = new Setting(pot.discVars.length, 0, 0)
  }


  def createFactor(pot: Pot) = new FactorType(pot)

  type NodeContent = Any
  type ContNodeContent = Nothing
  type VectNodeContent = Nothing
  class DiscNodeContent(var belief: Array[Double])
  def createDiscNodeContent(variable: DiscVar[Any]) = new DiscNodeContent(Array.ofDim[Double](variable.dom.size))
  def createContNodeContent(contVar: ContVar) = sys.error("Can't do brute force with continuous variables")
  def createVectNodeContent(vectVar: VectVar) = sys.error("Can't do brute force with vector variables")
  def acceptPotential = { case p: Potential2 => p }
  type Pot = Potential2


  val expFamStats = factors.map(f => f -> f.processor).collect({ case (f, s: Statistics) => (f, s) })

  def loopOverSettings(nodes: List[DiscNode], loop: (() => Unit) => Unit = body => body()): (() => Unit) => Unit = {
    nodes match {
      case Nil => (body: () => Unit) => loop(body)
      case head :: tail =>
        def newLoop(body: () => Unit) {
          if (!head.observed) for (setting <- head.variable.dom.indices) {
            for (edge <- head.edges) edge.factor.setting.disc(edge.index) = setting
            head.setting = setting
            loop(body)
          } else loop(body)
        }
        loopOverSettings(tail, newLoop)
    }
  }

  def currentScore(weights: FactorieVector) = {
    factors.iterator.map(f => f.processor.score(f.setting)).sum
  }

  def syncFactorObservations(): Unit = {
    for (n <- discNodes; if n.observed; e <- n.edges) e.factor.setting.disc(e.index) = n.setting
  }

  def inferMAP(weights: FactorieVector = new DenseTensor1(0)): MAPResult = {

    //initialize observations in factor settings
    syncFactorObservations()

    val nodes = var2DiscNode.values.toList
    val loop = loopOverSettings(nodes)
    var maxScore = Double.NegativeInfinity
    for (n <- nodes) fill(n.content.belief, Double.NegativeInfinity)

    var maxSetting: Array[Int] = null
    loop { () =>
      val score = currentScore(weights)
      for (n <- nodes) {
        n.content.belief(n.setting) = math.max(score, n.content.belief(n.setting))
      }

      if (score > maxScore) {
        maxScore = score
        maxSetting = nodes.view.map(_.setting).toArray
      }
    }

    for ((s, n) <- maxSetting zip nodes; if !n.observed) {
      n.setting = s
      maxNormalize(n.content.belief)
    }

    val gradient = new SparseVector(1000)
    for ((f, s) <- expFamStats)
      gradient += s.stats(f.setting)

    val state = nodes.map(n => n.variable -> n.variable.dom(n.setting))
    val maxMarginals = nodes.map(n =>
      DiscBelief(n.variable) -> Distribution.disc(n.variable.dom, n.content.belief.map(math.exp)))
    MAPResult(new MapBasedState(state.toMap), maxScore, gradient, new MapBasedState(maxMarginals.toMap))
  }

  def inferMarginals(weights: FactorieVector = new DenseTensor1(0)): MarginalResult = {
    syncFactorObservations()

    val nodes = var2DiscNode.values.toList
    val loop = loopOverSettings(nodes)
    var Z = 0.0
    for (n <- nodes) fill(n.content.belief, 0.0)

    loop { () =>
      val score = currentScore(weights)
      val prob = math.exp(score)
      for (n <- nodes) {
        n.content.belief(n.setting) += prob
      }
      Z += prob
    }

    for (n <- nodes) {
      normalize(n.content.belief)
      log(n.content.belief)
    }

    val logZ = math.log(Z)
    val gradient = new SparseVector(1000)

    loop { () =>
      val score = currentScore(weights)
      val prob = math.exp(score) / Z
      for ((f, s) <- expFamStats)
        gradient +=(s.stats(f.setting), prob)
    }

    val marginals = nodes.map(n =>
      DiscBelief(n.variable) -> Distribution.disc(n.variable.dom, n.content.belief.map(math.exp)))
    MarginalResult(logZ, gradient, new MapBasedState(marginals.toMap))
  }


}

